{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Analyzer where

import Prelude hiding (mapM, mapM_, forM_, foldlM, foldrM, sequence)

import Control.Applicative
import Control.Arrow (first, second)
import Control.Lens
import Control.Monad (liftM, liftM2, when, unless)
import Control.Monad.State.Strict (MonadState, StateT, execStateT,
                                   get, gets, modify)

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Sequence (ViewL(..), ViewR(..))
import Data.Traversable

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Language.LCC.AST
import Language.LCC.Parser
import Language.LCC.Target
import Language.LCC.TypeChecker
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


analyze :: (Applicative m, Err.ErrorM m, Target t)
        => t
        -> RawLocale
        -> m AnalyzedLocale
analyze target rawLocale = do
    withAbsolutePaths <- toAbsolute rawLocale

    withBuiltins <- injectBuiltins target withAbsolutePaths

    verifyNameLookup withBuiltins

    analyzed <- inferReturnTypes withBuiltins

    typeCheck analyzed

    return analyzed



toAbsolute :: (Applicative m, Err.ErrorM m, Show ret)
           => RelLocale ret -> m (AbsLocale ret)
toAbsolute = localeAST.traverse %%~ (./> trImpl.traverse %%~ toAbsPath)
  where
    toAbsPath :: (Err.ErrorM m, Scoped path ret m, Show path, Show ret)
              => RelativeVarPath -> m AbsoluteVarPath
    toAbsPath (RVAbsolutePath path) = return $ VAbsolutePath path
    toAbsPath (RVParamName name)    = return $ VParamName name
    toAbsPath rp@(RVRelativePath path) = do
        relTo <- viewS $ trSig.sigPath.absolute

        liftM VAbsolutePath (toAbsPath' relTo path)
      where
        toAbsPath' :: (Err.ErrorM m, Scoped path ret m, Show path, Show ret)
                   => Seq.Seq PathNode -> Seq.Seq PathNode -> m (Seq.Seq PathNode)
        toAbsPath' relTo path =
          case Seq.viewl path of

            Seq.EmptyL -> return relTo

            "^" :< pathTail ->
              case Seq.viewr relTo of
                Seq.EmptyR           -> Err.invalidRelativePath rp

                relToInit:>relToLast -> toAbsPath' relToInit pathTail

            down :< pathTail -> toAbsPath' (relTo|>down) pathTail



verifyNameLookup :: (Err.ErrorM m, Show ret) => AbsLocale ret -> m ()
verifyNameLookup l = scopedMapM_ (mapM_ verifyPath . view trImpl)
                                 (l^.localeAST)
  where
    verifyPath :: (Err.ErrorM m, ScopedAbs ret m, Show ret)
               => AbsoluteVarPath -> m ()
    verifyPath path = do
        found <- lookupPath path

        unless found
               (Err.symbolNotFound path)

    lookupPath :: ScopedAbs ret m => AbsoluteVarPath -> m Bool
    lookupPath path =
        getS >>= \t -> return $
          case path of
            VAbsolutePath p -> has (localeAST.atPath (p^.from absolute)._Just) l
            VParamName name -> has _Just $ lookupParam t name



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
                                        (known, [])

       if length unknown == length unknown'
         then Err.cycle unknown'
         else return (known', unknown')

    inferPass :: (Err.ErrorM m, MonadState InferIterData m)
              => AbsTranslation UnknownType
              -> m ()
    inferPass t = do
        exprType <- exprType'
        retType <- t ./> exprType . view trImpl

        modify $
          case retType of
            Nothing -> second (t:)

            Just ret -> let setter = at (t^.trSig.sigPath)
                            newVal = t & trSig.sigReturn .~ ret
                        in first (setter <>~ Just [newVal])

    revForM_ :: Monad m => [a] -> (a -> m b) -> m ()
    revForM_ xs f = foldrM (\x () -> f x >> return ()) () xs

    exprType' :: ( MonadState InferIterData st
                 , Err.ErrorM m
                 , ScopedAbs UnknownType m
                 )
              => st (AbsExpr -> m (Maybe Type))
    exprType' = liftM2 exprType folder' getSigReturn'

    folder' :: ( MonadState InferIterData st
               , Err.ErrorM m
               , ScopedAbs UnknownType m
               )
            => st (TypeFolder m)
    folder' = liftM foldInferNothrow getSigReturn'

    getSigReturn' :: (MonadState InferIterData st , ScopedAbs UnknownType m)
                  => st (SigReturnGetter m)
    getSigReturn' = liftM (mkSigReturnGetter . mapLookup) $ gets fst



typeCheck :: Err.ErrorM m => AbsLocale Type -> m ()
typeCheck l = scopedMapM_ checkLeaf ast
  where
    checkLeaf :: ExprTypeM Type m => AbsTranslation Type -> m ()
    checkLeaf tr = do
      ret <- exprType' (tr^.trImpl)

      when (isNothing ret)
           (Err.panic "typeCheck: exprType returned Nothing")

    exprType' :: ExprTypeM Type m => AbsExpr -> m (Maybe Type)
    exprType' = exprType (foldCheck getSigReturn) getSigReturn

    getSigReturn :: ExprTypeM Type m => SigReturnGetter m
    getSigReturn = maybeToThrow . mkSigReturnGetter . astLookup $ ast

    ast :: AbsAST Type
    ast = l^.localeAST
