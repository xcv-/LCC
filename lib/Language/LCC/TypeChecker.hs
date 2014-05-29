{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module Language.LCC.TypeChecker where

import Prelude hiding (mapM, sequence, all)

import Control.Applicative
import Control.Lens
import Control.Monad (join, when, liftM, liftM2, (>=>))
import Control.Monad.Error (throwError, catchError)
import Control.Monad.Reader (ask)

import Data.Foldable
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Traversable

import Text.Printf (printf)

import Language.LCC.Types
import qualified Language.LCC.Error as Err


typeOf :: (Err.ErrorM m, ScopedAbs ret m, Show ret)
       => AbsExpr -> AbsAST Type -> m Type
typeOf expr ast =
    exprType' expr >>= maybe err return
  where
    err = Err.panic ("typeOf: Could not resolve type of " ++ show expr)

    exprType' = exprType fold getSigReturn
    fold = foldInfer getSigReturn
    getSigReturn = maybeToThrow . mkSigReturnGetter . astLookup $ ast


type ExprTypeM ret m = (Err.ErrorM m, ScopedAbs ret m, Show ret)

type TypeFolder m = Maybe Type -> [AbsExpr] -> m (Maybe Type)

type SigReturnGetter m = AbsoluteVarPath -> [Maybe Type] -> m (Maybe Type)


exprType :: ExprTypeM ret m
         => TypeFolder m
         -> SigReturnGetter m
         -> AbsExpr
         -> m (Maybe Type)
exprType fold getSigReturn expr = do

  --this <- viewS (trSig.sigPath.to show)

  --traceM $ this ++ "\t\t\t" ++ show expr

  case () of
   ()| is _IntL    -> return (Just TInt)
     | is _DoubleL -> return (Just TDouble)
     | is _BoolL   -> return (Just TBool)
     | is _CharL   -> return (Just TChar)
     | is _StringL -> return (Just TString)
     | is _SConcat -> do
         fold (Just TString) (expr^?!_SConcat) >>= \case
           (Just TString) -> return (Just TString)
           (Just other)   -> Err.typeError TString other
           Nothing        -> Err.panic "exprType: fold returned Nothing"

     | is _Array ->
         (liftM.fmap) TArray $ fold Nothing (expr^?!_Array)

     | is _Cond -> do
         let (condition, ifT, ifF) = expr^?!_Cond
         fold (Just TBool) [condition]
         fold Nothing [ifT, ifF]

     | is _Funcall -> do
         let (fnPath, args) = expr^?!_Funcall
         argTypes <- mapM (exprType fold getSigReturn) args

         getSigReturn fnPath argTypes

     | is _Builtin -> return $ Just (expr^?!_Builtin.sigReturn)
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

mapLookup :: FlatASTMap path Type -> AbsolutePath -> [Translation path Type]
mapLookup astMap path = astMap ^.. ix path.traverse

astLookup :: AST path Type -> AbsolutePath -> [Translation path Type]
astLookup ast path = ast ^.. atPath path._Just._Leaf.traverse


mkSigReturnGetter :: Scoped path ret m
                  => (AbsolutePath -> [Translation path Type])
                  -> SigReturnGetter m
mkSigReturnGetter lookupPath path paramTypes =
    case path of
      VAbsolutePath path ->
        let translations = lookupPath (path^.from absolute)
            matching     = filter (\tr -> partMatchParams1 paramTypes
                                                           (tr^..trParamTypes))
                                  translations
        in return $ matching^?_Single.trSig.sigReturn

      VParamName name ->
        previewS $ trSig.sigParams.folded.filtered (matchName name).paramType
  where
    matchName :: String -> Param -> Bool
    matchName name p = p^.paramName == name


maybeToThrow :: ExprTypeM ret m => SigReturnGetter m -> SigReturnGetter m
maybeToThrow maybeLookup path paramTypes =
    maybeLookup path paramTypes >>= \case
      Just t  ->
        return (Just t)

      Nothing ->
        maybe (Err.panic $ printf
                "maybeToThrow: Missing parameter types for lookup: %s(%s)"
                  (show path) (intercalate ", " $ map show paramTypes))
              (Err.signatureNotFound path)
              (sequence paramTypes)


{-
throwToMaybe :: Err.ErrorM m
             => SigReturnGetter m Identity
             -> SigReturnGetter m Maybe
throwToMaybe throwLookup path paramTypes = do
    let paramTypes' = map (Just . runIdentity) paramTypes

    liftM Just (throwLookup path paramTypes')
      `catchError` \e -> return Nothing
      -}
