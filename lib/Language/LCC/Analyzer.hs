{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Analyzer where

import Prelude hiding (mapM, mapM_, forM_, foldlM, foldrM, sequence)

import Control.Applicative
import Control.Arrow (first, second)
import Control.Lens hiding (Identity(..), runIdentity)
import Control.Monad (liftM, liftM2)
import Control.Monad (unless, ap, liftM)
import Control.Monad.Identity (Identity(..), runIdentity)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (MonadState, StateT, execStateT,
                                   get, gets, modify)

import Data.Foldable
import Data.Semigroup
import Data.Sequence (ViewL(..), ViewR(..))
import Data.Traversable

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Language.LCC.Types

import Language.LCC.Parser
import Language.LCC.Target
import qualified Language.LCC.Error as Err


{-
checkInterfaces :: [(Locale, LocaleState)]
                -> Either LocaleError [(Locale, LocaleState)]
checkInterfaces [] = Right []
checkInterfaces states =
    zipWithM applyCheck states (tail $ cycle states)
  where
    applyCheck (lc1, st1) (lc2, st2) =
        case checkInterface st1 st2 of
          ([],[]) -> Right (lc1, st1)
          ([],missing) -> Left $ LocaleInterfaceError (lcName lc2) missing
          (missing, _) -> Left $ LocaleInterfaceError (lcName lc1) missing

checkInterface :: LocaleState -> LocaleState
               -> ([TranslationSignature], [TranslationSignature])
checkInterface st1 st2 =
    (sigs2 \\ sigs1, sigs1 \\ sigs2)
  where
    sigs1 = Set.toList $ envSignatures $ lcsEnv st1
    sigs2 = Set.toList $ envSignatures $ lcsEnv st2
    -}


compile :: (Applicative m, Err.ErrorM m, Target t)
        => t
        -> RawLocale
        -> m AnalyzedLocale
compile target rawLocale = do
    withAbsolutePaths <- toAbsolute rawLocale

    withBuiltins <- injectBuiltins target withAbsolutePaths

    verifyNameLookup withBuiltins

    analyzed <- inferReturnTypes withBuiltins

    typeCheck analyzed

    return analyzed



toAbsolute :: (Applicative m, Err.ErrorM m, Show ret)
           => RelLocale ret
           -> m (AbsLocale ret)
toAbsolute = localeAST.traverse %%~ (./> trImpl.traverse %%~ toAbsPath)
  where
    toAbsPath :: (Applicative m, Err.ErrorM m, Show path, Show ret)
              => RelativeVarPath
              -> ScopedAST path ret m AbsoluteVarPath
    toAbsPath (RVAbsolutePath path) = return $ VAbsolutePath path
    toAbsPath (RVParamName name)    = return $ VParamName name
    toAbsPath rp@(RVRelativePath path) = do
        relTo <- asks $ view (scopeData.tsd.trSignature.sigPath.absolute)

        VAbsolutePath <$> toAbsPath' relTo path
      where
        toAbsPath' :: (Err.ErrorM m, Show path, Show ret)
                   => Seq.Seq PathNode
                   -> Seq.Seq PathNode
                   -> ScopedAST path ret m (Seq.Seq PathNode)
        toAbsPath' relTo path =
          case Seq.viewl path of

            Seq.EmptyL -> return relTo

            "^" :< pathTail ->
              case Seq.viewr relTo of
                Seq.EmptyR           -> Err.invalidRelativePath rp

                relToInit:>relToLast -> toAbsPath' relToInit pathTail

            down :< pathTail -> toAbsPath' (relTo|>down) pathTail



verifyNameLookup :: (Functor m, Err.ErrorM m, Show ret) => AbsLocale ret -> m ()
verifyNameLookup l = forM_ (l^.localeAST)
                           (./> mapM_ verifyPath . view trImpl)
  where
    verifyPath :: (Functor m, Err.ErrorM m, Show ret)
               => AbsoluteVarPath
               -> ScopedAbsAST ret m ()
    verifyPath path = do
        found <- lookupPath path

        unless found
          (Err.symbolNotFound path)

    lookupPath :: (Functor m, Monad m)
               => AbsoluteVarPath
               -> ScopedAbsAST ret m Bool
    lookupPath path = do
        asks (view $ scopeData.tsd) <&> \t ->
          case path of
            (VAbsolutePath path) -> has (localeAST.atPath (path^.from absolute)) l
            (VParamName name)    -> has _Just $ lookupParam t name



type InferIterData = (FlatAbsASTMap Type, [AbsTranslation UnknownType])

inferReturnTypes :: Err.ErrorM m => AbsLocale UnknownType -> m AnalyzedLocale
inferReturnTypes l =
    findResult . iterate (>>=iter) . prepare $ l
  where

    prepare :: Monad m => AbsLocale UnknownType -> m InferIterData
    prepare l = return (Map.empty,  l^..localeAST.traverse)

    findResult :: Monad m => [m InferIterData] -> m AnalyzedLocale
    findResult (mx:mxs) = mx >>= \case
        (known, []) -> return $ localeAST .~ rebuild (Map.toList known) $ l
        _           -> findResult mxs

    iter :: Err.ErrorM m => InferIterData -> m InferIterData
    iter (known, unknown) = do
       (known', unknown') <- execStateT (revForM_ unknown inferPass)
                                        (known, unknown)

       if Map.size known == Map.size known'
         then throwError $ Err.Cycle unknown'
         else return (known', unknown')

    inferPass :: (Err.ErrorM m, MonadState InferIterData m)
              => AbsTranslation UnknownType
              -> m ()
    inferPass t = do
        exprType <- exprType'
        retType <- t ./> exprType . view trImpl

        modify $ case unwrapMaybe retType of
            Nothing -> second (t:)

            Just ret -> let setter = at (t^.trSignature.sigPath)
                            newVal = t & trSignature.sigReturn .~ ret
                        in first (setter .~ Just newVal)

    revForM_ :: Monad m => [a] -> (a -> m b) -> m ()
    revForM_ xs f = foldrM (\x () -> f x >> return ()) () xs

    exprType' :: (MonadState InferIterData st, Err.ErrorM m)
              => st (AbsExpr -> ScopedAbsAST UnknownType m (WrappedMaybe Type))
    exprType' = liftM2 exprType folder' getSigReturn'

    folder' :: (Monad m, MonadState InferIterData st)
            => st (TypeFolder m WrappedMaybe)
    folder' = liftM foldInfer getSigReturn'

    getSigReturn' :: (Monad m, MonadState InferIterData st)
                  => st (SigReturnGetter m WrappedMaybe)
    getSigReturn' = do
        gsr <- liftM mapLookupMaybe (gets fst)

        return $ \path -> liftM WrapMaybe . gsr path . map unwrapMaybe




typeCheck :: Err.ErrorM m => AbsLocale Type -> m ()
typeCheck l = scopedMapM_ checkLeaf ast
  where
    ast :: AbsAST Type
    ast = l^.localeAST

    getSigReturn :: SigReturnGetter m Identity
    getSigReturn = astLookupOrThrow ast

    checkLeaf :: ExprTypeConstr m Identity
              => AbsTranslation Type
              -> ScopedAbsAST Type m (Identity Type)
    checkLeaf = exprType (foldCheck getSigReturn) getSigReturn . _trImpl



type ExprTypeConstr m f = ( Err.ErrorM m
                          , Traversable f
                          , Applicative f
                          , Semigroup (f Type)
                          )

type TypeFolder m f = f Type
                    -> [AbsExpr]
                    -> ScopedAbsAST Type m (f Type)

type SigReturnGetter m f = AbsoluteVarPath
                        -> [f Type]
                        -> ScopedAbsAST Type m (f Type)


exprType :: ExprTypeConstr m f
         => TypeFolder m f
         -> SigReturnGetter m f
         -> AbsExpr
         -> ScopedAbsAST ret m (f Type)
exprType fold getSigReturn expr
  | is _IntL    = return (pure TInt)
  | is _DoubleL = return (pure TDouble)
  | is _BoolL   = return (pure TBool)
  | is _CharL   = return (pure TChar)
  | is _StringL = return (pure TString)
  | is _SConcat = do
      ret <- fold (pure TString) (expr^?!_SConcat)

      sequenceA $ ret <&> \case
        TString -> return TString
        other   -> Err.typeError TString other

  | is _Array =
      (fmap.fmap) TArray $ fold Nothing (expr^?!_Array)

  | is _Cond = do
      let (condition, ifT, ifF) = expr^?!_Cond
      fold (Just TBool) [condition]
      fold Nothing [ifT, ifF]

  | is _Funcall = do
      let (fnPath, args) = expr^?!_Funcall
      argTypes <- mapM (exprType fold getSigReturn) args

      sequence $ getSigReturn fnPath argTypes

  | is _Builtin = return $ pure (expr^?!_Builtin)
  where
    is prism = has prism expr



-- TypeFolders

foldInfer :: Semigroup (f Type) => SigReturnGetter m f -> TypeFolder m f
foldInfer getSigReturn t exprs = do
    types <- forM exprs $ exprType (foldInfer getSigReturn) getSigReturn

    foldl' (<>) t types



foldCheck :: Semigroup (f [Type]) => SigReturnGetter m f -> TypeFolder m f
foldCheck getSigReturn t exprs = do
    types <- forM exprs $ exprType (foldCheck getSigReturn) getSigReturn

    sequence $ foldl' checkEqual [t] $ (map.fmap) (:[]) types
  where
    checkEqual expected found = sequence $ (expected <> found) <&> \case
        [t]   -> return t

        [t,u]
          | t == u    -> return t
          | otherwise -> Err.typeError t u

        ts    -> Err.panic $ "foldCheck: Unexpected result length =" ++ show (length ts)



-- SigReturnGetters

mapLookupMaybe :: FlatAbsASTMap Type -> SigReturnGetter m Maybe
mapLookupMaybe astMap path paramTypes =
    return $ view (trSignature.sigReturn) <$> Map.lookup path astMap


mapLookupOrThrow :: FlatAbsASTMap Type -> SigReturnGetter m Identity
mapLookupOrThrow astMap path paramTypes = do
    let paramTypes' = map (Just . runIdentity) paramTypes

    mapLookupMaybe astMap path paramTypes' >>= \case
      Just t  -> return $ Identity t
      Nothing -> Err.signatureNotFound path (map runIdentity paramTypes)


astLookupMaybe :: AbsAST Type -> SigReturnGetter m Maybe
astLookupMaybe ast path paramTypes =
    return $ view (trSignature.sigReturn) <$> ast ^? atPath path


astLookupOrThrow :: AbsAST Type -> SigReturnGetter m Identity
astLookupOrThrow ast path paramTypes = do
    let paramTypes' = map (Just . runIdentity) paramTypes

    astLookupMaybe ast path paramTypes' >>= \case
      Just t  -> return $ Identity t
      Nothing -> Err.signatureNotFound path (map runIdentity paramTypes)


-- Instances

instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)


newtype WrappedMaybe a = WrapMaybe { unwrapMaybe :: Maybe a }
    deriving (Eq, Ord, Monoid, Functor, Applicative, Foldable, Traversable)

instance Semigroup (WrappedMaybe a) where
    (<>) = mappend
