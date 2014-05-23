{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module Language.LCC.TypeChecker where

import Prelude hiding (mapM, sequence)

import Control.Applicative
import Control.Lens
import Control.Monad (join, liftM, liftM2, (>=>))
import Control.Monad.Error (throwError, catchError)
import Control.Monad.Reader (ask)

import Data.Foldable
import Data.List (intercalate)
import Data.Monoid
import Data.Traversable

import Text.Printf (printf)

import Language.LCC.Types
import qualified Language.LCC.Error as Err


type ExprTypeM ret m = (Err.ErrorM m, ScopedAbs ret m, Show ret)

type TypeFolder m = Maybe Type -> [AbsExpr] -> m (Maybe Type)

type SigReturnGetter m = AbsoluteVarPath -> [Maybe Type] -> m (Maybe Type)


exprType :: ExprTypeM ret m
         => TypeFolder m
         -> SigReturnGetter m
         -> AbsExpr
         -> m (Maybe Type)
exprType fold getSigReturn expr
  | is _IntL    = return (pure TInt)
  | is _DoubleL = return (pure TDouble)
  | is _BoolL   = return (pure TBool)
  | is _CharL   = return (pure TChar)
  | is _StringL = return (pure TString)
  | is _SConcat = do
      fold (Just TString) (expr^?!_SConcat) >>= \case
        (Just TString) -> return (pure TString)
        (Just other)   -> Err.typeError TString other
        Nothing        -> Err.panic "exprType: fold returned Nothing"

  | is _Array =
      (liftM.fmap) TArray $ fold Nothing (expr^?!_Array)

  | is _Cond = do
      let (condition, ifT, ifF) = expr^?!_Cond
      fold (Just TBool) [condition]
      fold Nothing [ifT, ifF]

  | is _Funcall = do
      let (fnPath, args) = expr^?!_Funcall
      argTypes <- mapM (exprType fold getSigReturn) args

      getSigReturn fnPath argTypes

  | is _Builtin = return $ pure (expr^?!_Builtin)
  where
    is prism = has prism expr



-- TypeFolders

foldInfer :: ExprTypeM ret m => SigReturnGetter m -> TypeFolder m
foldInfer getSigReturn known =
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

mapLookup :: FlatAbsASTMap Type -> AbsolutePath -> [Maybe Type] -> Maybe Type
mapLookup astMap path paramTypes = astMap ^? ix path.trSignature.sigReturn

astLookup :: AbsAST Type -> AbsolutePath -> [Maybe Type] -> Maybe Type
astLookup ast path paramTypes = ast ^? atPath path.trSignature.sigReturn


mkSigReturnGetter :: Scoped path ret m
                  => (AbsolutePath -> [Maybe Type] -> Maybe Type)
                  -> SigReturnGetter m
mkSigReturnGetter lookup path paramTypes = do
    ScopeData tr <- ask

    return $
      case path of
        VAbsolutePath path ->
          lookup (path^.from absolute) paramTypes

        VParamName name ->
          tr^?trSignature.sigParams.folded.filtered (matchParam name).paramType

  where
    matchParam :: String -> Param -> Bool
    matchParam name p = p^.paramName == name


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
