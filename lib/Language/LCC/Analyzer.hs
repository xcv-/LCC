{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Analyzer where

import Prelude hiding (mapM, mapM_, forM_, foldlM, foldrM, sequence)

import Control.Applicative hiding ((<|>), many)
import Control.Lens
import Control.Monad (unless, ap, liftM)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (MonadState, StateT, execStateT,
                                   get, gets, modify)

import Data.Foldable
import Data.Monoid
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

        case retType of
            Nothing -> modify $ \(known, unknown) -> (known, t:unknown)

            Just ret -> let setter = at (t^.trSignature.sigPath)
                            newVal = t & trSignature.sigReturn .~ ret
                        in modify $ \(known, unknown) ->
                              (known & setter .~ Just newVal, unknown)

    revForM_ :: Monad m => [a] -> (a -> m b) -> m ()
    revForM_ xs f = foldrM (\x () -> f x >> return ()) () xs

    exprType' :: (MonadState InferIterData st, Err.ErrorM m)
              => st (Expr AbsoluteVarPath
                     -> ScopedAbsAST UnknownType m (Maybe Type))
    exprType' = liftM2 exprType  folder' getSigReturn'

    folder' :: MonadState InferIterData st => st (TypeFolder m)
    folder' = liftM foldInfer getSigReturn'

    getSigReturn' :: MonadState InferIterData st => st (SigReturnGetter m Maybe)
    getSigReturn' = liftM mapLookupMaybe $ use _1






typeCheck :: Err.ErrorM m => AbsLocale ret -> m ()
typeCheck l = scopedMapM_ checkLeaf (l^.localeAST)
  where
    checkLeaf :: AbsTranslation ret -> m ()
    checkLeaf = exprType foldCheck True . _trImpl


inferType :: Err.ErrorM m => Expr AbsoluteVarPath -> m Type
inferType = exprType (foldInfer True) True



type ExprTypeConstr m f = (Err.ErrorM m, Traversable f, Applicative f)

type TypeFolder m = Maybe Type
                 -> [Expr AbsoluteVarPath]
                 -> ScopedAbsAST Type m (Maybe Type)

type SigReturnGetter m f = AbsoluteVarPath
                        -> [f Type]
                        -> ScopedAbsAST Type m (f Type)


exprType :: ExprTypeConstr m f
         => TypeFolder m
         -> SigReturnGetter m f
         -> Expr AbsoluteVarPath
         -> ScopedAbsAST ret1 m (f ret2)
exprType fold getSigReturn expr
  | is _IntL    = return TInt
  | is _DoubleL = return TDouble
  | is _BoolL   = return TBool
  | is _CharL   = return TChar
  | is _StringL = return TString
  | is _SConcat = do
      fold (Just TString) (expr^?!_SConcat) >>= \case
        (Just TString) -> return TString
        (Just other)   -> Err.typeError TString other
        Nothing        -> Err.panic "exprType: fold returned Nothing)"

  | is _Array =
      TArray <$> fold Nothing (expr^?!_Array)

  | is _Cond = do
      let (condition, ifT, ifF) = expr^?!_Cond
      fold (Just TBool) [condition]
      fold Nothing [ifT, ifF]

  | is _Funcall = do
      let (fnPath, args) = expr^?!_Funcall
      argTypes <- mapM (exprType fold getSigReturn) args

      sequence $ getSigReturn fnPath <$> argTypes

  | is _Builtin = return (expr^?!_Builtin)
  where
    is prism = has prism expr



-- TypeFolders

foldInfer :: SigReturnGetter m t -> TypeFolder m
foldInfer _            (Just known) _     = return (Just known)
foldInfer getSigReturn Nothing      exprs =
    let types = forM exprs $ exprType (foldInfer getSigReturn) getSigReturn
    in getFirst . foldMap First <$> types



foldCheck :: SigReturnGetter m t -> TypeFolder m
foldCheck _            expected []    = return expected
foldCheck getSigReturn expected exprs =
    let types = forM exprs $ exprType (foldCheck getSigReturn) getSigReturn
    in foldlM checkEqual expected <$> types
  where
    checkEqual :: Err.ErrorM m => Type -> Type -> m Type
    checkEqual Nothing         found        = return found
    checkEqual expected        Nothing      = return expected
    checkEqual (Just expected) (Just found)
      | expected == found = return (Just found)
      | otherwise         = Err.typeError expected found



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
