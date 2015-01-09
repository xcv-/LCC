{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Simplifier.RemoveUnused where

import Prelude hiding (any, mapM, concat)

import Control.Lens
import Control.Monad (liftM)

import Data.Foldable
import Data.Functor
import Data.Graph (Graph, Vertex, graphFromEdges, transposeG, reachable)
import Data.Traversable

import Language.LCC.AST
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err


type Node = (AnalyzedTranslation, AnalyzedSignature, [AnalyzedSignature])
type V2N  = Vertex -> Node
type K2V  = AnalyzedSignature -> Maybe Vertex

removeUnused :: Err.ErrorM m => AnalyzedAST -> m AnalyzedAST
removeUnused ast = do
    graphData <- forM (toList ast) $ \t -> do
                   calls <- t ./> findCalls ast . view trImpl
                   return (t, t^.trSig, calls)

    let callGraph   = graphFromEdges graphData
        callerGraph = callGraph & _1 %~ transposeG

    return $ filterTree (isUsed callerGraph) ast


findCalls :: (Err.ErrorM m, ScopedAbs Type m)
           => AnalyzedAST
           -> AbsExpr
           -> m [AnalyzedSignature]
findCalls ast = \case
    Array arr  -> liftM concat $ mapM (findCalls ast) arr
    SConcat s  -> liftM concat $ mapM (findCalls ast) s
    Cond c t f -> liftM concat $ mapM (findCalls ast) [c,t,f]

    Funcall (Fn (VAbsolutePath path)) args ->
      let absPath = path^.from absolute
      in findFunction ast absPath args >>= \tr -> return [tr^.trSig]

    _ -> return []


isUsed :: (Graph, V2N, K2V) -> AnalyzedTranslation -> Bool
isUsed (g, v2n, k2v) tr =
    case reachable g <$> k2v (tr^.trSig) of
      Nothing -> error $ "isUsed: could not find vertex " ++ show (tr^.trSig)
      Just vs -> any (not . isPrivateTr . view _1 . v2n) vs
