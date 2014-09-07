{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module Language.LCC.TypeChecker where

import Prelude hiding (mapM, sequence, all)

import Control.Applicative
import Control.Lens
import Control.Monad (join, liftM, liftM2)

import Data.Foldable hiding (fold)
import Data.Traversable

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

  | is _Cond =
      let (condition, ifT, ifF) = expr^?!_Cond

      in fold (Just TBool) [condition]
      >> fold Nothing [ifT, ifF]

  | is _Funcall = do
      let (fn, args) = expr^?!_Funcall

      case fn of
        Builtin sig -> return (Just $ sig^.sigReturn)
        Input t _   -> return (Just t)
        Fn fnPath   -> do
          argTypes <- mapM (exprType fold getSigReturn) args
          (^?_Right) `liftM` getSigReturn fnPath argTypes

  | otherwise = error $ "exprType: unknown Expression: " ++ show expr
  where
    is p = has p expr



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
    altSkipErrors acc t =
      liftM2 (<|>) acc (t `Err.catching` \_ -> return Nothing)


foldCheck :: ExprTypeM ret m => SigReturnGetter m -> TypeFolder m
foldCheck getSigReturn expected exprs =
    foldl' (\acc x -> join $ liftM2 checkEqual acc x)
           (return expected)
           (map exprType' exprs)
  where
    exprType' = exprType (foldCheck getSigReturn) getSigReturn

    checkEqual :: ExprTypeM ret m => Maybe Type -> Maybe Type -> m (Maybe Type)
    checkEqual (Just expect) (Just found)
      | expect == found = return (Just found)
      | otherwise       = Err.typeError expect found

    checkEqual expect found = return (expect <|> found)



-- SigReturnGetters

mapLookup :: FlatASTMap path Type -> PathLookup path
mapLookup astMap path = astMap ^.. ix path.traverse

astLookup :: AST path Type -> PathLookup path
astLookup ast path = ast ^.. atPath path._Just._Leaf.traverse

paramLookup :: Scoped path ret m => String -> m (Maybe Param)
paramLookup name = previewS $ trSig.sigParams.folded.filtered matchName
  where
    matchName :: Param -> Bool
    matchName p = p^.paramName == name


mkSigReturnGetter :: Scoped path ret m
                  => PathLookup AbsoluteVarPath -> SigReturnGetter m
mkSigReturnGetter lookupPath path paramTypes =
    case path of
      VParamName name ->
        maybe (Left []) (^.paramType.re _Right) `liftM` paramLookup name

      VAbsolutePath absPath ->
        let translations = lookupPath (absPath^.from absolute)
            matched      = filter (\tr -> partMatchParams1 paramTypes
                                                           (tr^..trParamTypes))
                                  translations
        in return $ case matched of
                      [tr] -> Right $ tr^.trSig.sigReturn
                      _    -> Left  $ matched


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
      `Err.catching` \e -> return Nothing
      -}
