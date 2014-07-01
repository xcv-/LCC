{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module Language.LCC.TypeChecker where

import Prelude hiding (mapM, sequence, all)

import Control.Applicative
import Control.Lens
import Control.Monad (join, when, liftM, liftM2, (>=>))
import Control.Monad.Except (throwError, catchError)
import Control.Monad.Reader (ask)

import Data.Foldable
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Traversable

import Text.Printf (printf)

import Language.LCC.AST
import qualified Language.LCC.Error as Err


typeOf :: (Err.ErrorM m, ScopedAbs ret m, Show ret)
       => AnalyzedAST
       -> AbsExpr
       -> m Type
typeOf ast expr =
    exprType' expr >>= maybe err return
  where
    err = Err.panic ("typeOf: Could not resolve type of " ++ show expr)

    exprType'    = exprType fold getSigReturn
    fold         = foldInfer getSigReturn
    getSigReturn = maybeToThrow . mkSigReturnGetter . astLookup $ ast


calleeSignature :: (Err.ErrorM m, ScopedAbs Type m)
                => AnalyzedAST
                -> AbsoluteVarPath
                -> [AbsExpr]
                -> m (Signature AbsoluteVarPath Type)
calleeSignature ast f args =
    case f of
      VParamName name -> do
        param <- paramLookup name
        case param of
          Just p -> return (paramSig p)
          Nothing -> Err.symbolNotFound f

      VAbsolutePath p -> do
        tr <- findFunction ast (p^.from absolute) args
        return $ tr^.trSig & sigPath %~ view (absolute . re _Absolute)


findFunction :: (Err.ErrorM m, ScopedAbs Type m)
             => AnalyzedAST
             -> AbsolutePath
             -> [AbsExpr]
             -> m AnalyzedTranslation
findFunction ast f args = do
    argTypes <- mapM (typeOf ast) args

    let filterMatching = filter (\tr -> tr^..trParamTypes == argTypes)
        singleMatch    = preview _Single . filterMatching

    case singleMatch $ astLookup ast f of
      Just tr -> return tr
      Nothing -> Err.signatureNotFound (f^.absolute.re _Absolute) argTypes



type ExprTypeM ret m = (Err.ErrorM m, ScopedAbs ret m, Show ret)

type TypeFolder m = Maybe Type -> [AbsExpr] -> m (Maybe Type)

type PathLookup path = AbsolutePath -> [Translation path Type]

type SigReturnGetter m = AbsoluteVarPath
                       -> [Maybe Type]
                       -> m (Either [AnalyzedTranslation] Type)


exprType :: ExprTypeM ret m
         => TypeFolder m
         -> SigReturnGetter m
         -> AbsExpr
         -> m (Maybe Type)
exprType fold getSigReturn expr
  -- | trace ("exprType " ++ show expr) False = undefined
  | is _IntL    = return (Just TInt)
  | is _DoubleL = return (Just TDouble)
  | is _BoolL   = return (Just TBool)
  | is _CharL   = return (Just TChar)
  | is _StringL = return (Just TString)
  | is _SConcat =
      fold (Just TString) (expr^?!_SConcat) >>= \case
        (Just TString) -> return (Just TString)
        (Just other)   -> Err.typeError TString other
        Nothing        -> Err.panic "exprType: fold returned Nothing"

  | is _Array =
      (liftM.fmap) TArray $ fold Nothing (expr^?!_Array)

  | is _Cond = do
      let (condition, ifT, ifF) = expr^?!_Cond
      fold (Just TBool) [condition]
      fold Nothing [ifT, ifF]

  | is _Funcall = do
      let (fn, args) = expr^?!_Funcall

      case fn of
        Builtin sig -> return (Just $ sig^.sigReturn)
        Fn fnPath   -> do
          argTypes <- mapM (exprType fold getSigReturn) args
          (^?_Right) `liftM` getSigReturn fnPath argTypes
  where
    is prism = has prism expr



-- TypeFolders

foldInfer :: ExprTypeM ret m => SigReturnGetter m -> TypeFolder m
foldInfer getSigReturn known =
    foldl' (liftM2 (<|>)) (return known) . map exprType'
  where
    exprType' = exprType (foldInfer getSigReturn) getSigReturn


foldInferNothrow :: ExprTypeM ret m => SigReturnGetter m -> TypeFolder m
foldInferNothrow getSigReturn known =
    foldl' altSkipErrors (return known) . map exprType'
  where
    exprType' = exprType (foldInfer getSigReturn) getSigReturn

    altSkipErrors :: Err.ErrorM m
                  => m (Maybe Type) -> m (Maybe Type) -> m (Maybe Type)
    altSkipErrors acc t = liftM2 (<|>) acc (t `catchError` \e -> return Nothing)


foldCheck :: ExprTypeM ret m => SigReturnGetter m -> TypeFolder m
foldCheck getSigReturn expected exprs =
    foldl' (\acc x -> join $ liftM2 checkEqual acc x)
           (return expected)
           (map exprType' exprs)
  where
    exprType' = exprType (foldCheck getSigReturn) getSigReturn

    checkEqual :: ExprTypeM ret m => Maybe Type -> Maybe Type -> m (Maybe Type)
    checkEqual (Just expected) (Just found)
      | expected == found = return (Just found)
      | otherwise         = Err.typeError expected found

    checkEqual expected found = return (expected <|> found)



-- SigReturnGetters



mapLookup :: FlatASTMap path Type -> PathLookup path
mapLookup astMap path = astMap ^.. ix path.traverse

astLookup :: AST path Type -> PathLookup path
astLookup ast path = ast ^.. atPath path._Just._Leaf.traverse

paramLookup :: Scoped path ret m => String -> m (Maybe Param)
paramLookup name = previewS $ trSig.sigParams.folded.filtered (matchName name)
  where
    matchName :: String -> Param -> Bool
    matchName name p = p^.paramName == name


mkSigReturnGetter :: Scoped path ret m
                  => PathLookup AbsoluteVarPath -> SigReturnGetter m
mkSigReturnGetter lookupPath path paramTypes =
    case path of
      VParamName name ->
        maybe (Left []) (^.paramType.re _Right) `liftM` paramLookup name

      VAbsolutePath path ->
        let translations = lookupPath (path^.from absolute)
            matching     = filter (\tr -> partMatchParams1 paramTypes
                                                           (tr^..trParamTypes))
                                  translations
        in return $ case matching of
                      [tr] -> Right $ tr^.trSig.sigReturn
                      _    -> Left  $ matching


maybeToThrow :: ExprTypeM ret m => SigReturnGetter m -> SigReturnGetter m
maybeToThrow maybeLookup path paramTypes =
    maybeLookup path paramTypes >>= \case
      Left [] -> Err.signatureNotMatched path paramTypes
      Right t -> return (Right t)
      Left ts -> Err.ambiguousOverloads path paramTypes ts


{-
throwToMaybe :: Err.ErrorM m
             => SigReturnGetter m Identity
             -> SigReturnGetter m Maybe
throwToMaybe throwLookup path paramTypes = do
    let paramTypes' = map (Just . runIdentity) paramTypes

    liftM Just (throwLookup path paramTypes')
      `catchError` \e -> return Nothing
      -}
