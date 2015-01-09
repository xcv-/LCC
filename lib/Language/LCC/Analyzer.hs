{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Analyzer where

import Prelude hiding (mapM, mapM_, sequence)

import GHC.Exts (fromList)

import Control.Applicative
import Control.Arrow (first, second)
import Control.Lens
import Control.Monad (liftM, liftM2, when, unless)
import Control.Monad.State.Strict (MonadState, execStateT, gets, modify)

import Data.Foldable
import Data.Maybe
import Data.Sequence (ViewL(..), ViewR(..))

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Text.Parsec.Pos

import Language.LCC.AST
import Language.LCC.Target
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err


resolveImports :: (Applicative m, Err.ErrorM m, Target t)
               => t
               -> [(Bool, RawLocale, [Import])]
               -> m [RawLocale]
resolveImports _ ls = do
    let withImports = [undefined | (_,_,imports) <- ls, not (null imports)]

    unless (null withImports) $
      Err.globalPanic "Imports aren't implemented yet"

    return [l | (False, l, _) <- ls]


analyze :: (Applicative m, Err.ErrorM m, Target t)
        => t
        -> RawLocale
        -> m AnalyzedLocale
analyze target rawLocale = do
    withAbsolutePaths <- toAbsolute rawLocale

    withInputs <- injectInputs withAbsolutePaths

    withBuiltins <- injectBuiltins target withInputs

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
    toAbsPath rp@(RVRelativePath relP) = do
        relTo <- viewS $ trSig.sigPath.absolute

        liftM VAbsolutePath (toAbsPath' relTo relP)
      where
        toAbsPath' :: (Err.ErrorM m, Scoped path ret m, Show path, Show ret)
                   => Seq.Seq PathNode -> Seq.Seq PathNode -> m (Seq.Seq PathNode)
        toAbsPath' relTo path =
          case Seq.viewl path of

            Seq.EmptyL -> return relTo

            "^" :< pathTail ->
              case Seq.viewr relTo of
                Seq.EmptyR     -> Err.invalidRelativePath rp

                relToInit :> _ -> toAbsPath' relToInit pathTail

            down :< pathTail -> toAbsPath' (relTo|>down) pathTail



addTranslation :: (Applicative m, Err.ErrorM m, Eq path, Show path, Show ret)
               => Translation path ret
               -> Locale path ret
               -> m (Locale path ret)
addTranslation tr =
    localeAST.atPath (tr^.trSig.sigPath) %%~ \case
      Just (Subtree _) ->
        Err.globalPanic $
          "Cannot insert translation: subtree exists at the same path: "
            ++ show (tr^.trSig.sigPath)

      node ->
        Just . Leaf <$> insertIntoLeaf (node^?_Just._Leaf)
  where
    insertIntoLeaf prevs =
        case find (matchTrParams tr) =<< prevs of
          Nothing    -> return $ tr : (prevs^.._Just.traverse)
          Just confl -> Err.conflict [confl, tr]


injectInputs :: (Applicative m, Err.ErrorM m)
             => AbsLocale UnknownType
             -> m (AbsLocale UnknownType)
injectInputs l =
    foldrM (addTranslation . tr) l (l^.localeInputs)
  where
    tr (LocaleInput p anns) =
         Translation
           { _trSig  = sig p
           , _trImpl = Funcall (Input (p^.paramType) (p^.paramName)) []
           , _trAnnotations = anns
           , _trSourcePos   = newPos "<input>" 0 0
           }

    sig p = Signature
              { _sigPath   = fromList [p^.paramName]
              , _sigParams = []
              , _sigReturn = UnknownType
              }


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
    findResult $ iterate (>>=iter) prepared
  where
    prepared :: Monad m => m InferIterData
    prepared = return (Map.empty,  l^..localeAST.traverse)

    findResult :: Monad m => [m InferIterData] -> m AnalyzedLocale
    findResult []       = error "Analyzer: findResult: empty list"
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
        exprType' <- mexprType
        retType   <- t ./> exprType' . view trImpl

        modify $
          case retType of
            Nothing -> second (t:)

            Just ret -> let setter = at (t^.trSig.sigPath)
                            newVal = t & trSig.sigReturn .~ ret
                        in first (setter <>~ Just [newVal])

    revForM_ :: Monad m => [a] -> (a -> m b) -> m ()
    revForM_ xs f = foldrM (\x () -> f x >> return ()) () xs

    mexprType :: ( MonadState InferIterData st
                 , Err.ErrorM m
                 , ScopedAbs UnknownType m
                 )
              => st (AbsExpr -> m (Maybe Type))
    mexprType = liftM2 exprType mfolder mgetSigReturn

    mfolder :: ( MonadState InferIterData st
               , Err.ErrorM m
               , ScopedAbs UnknownType m
               )
            => st (TypeFolder m)
    mfolder = liftM foldInferNothrow mgetSigReturn

    mgetSigReturn :: (MonadState InferIterData st , ScopedAbs UnknownType m)
                  => st (SigReturnGetter m)
    mgetSigReturn = liftM (mkSigReturnGetter . mapLookup) $ gets fst



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
