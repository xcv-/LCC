{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Simplifier.RemoveUnused where

import Prelude hiding (any, mapM, concat)

import Control.Lens
import Control.Monad (liftM)
import Control.Monad.State.Strict (MonadState, execStateT, modify)

import Data.Foldable
import Data.Functor
import Data.Graph
import Data.Traversable

import qualified Data.Map.Lazy as Map

import Language.LCC.AST
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err


type GraphData = [(AnalyzedTranslation, AnalyzedSignature, [AnalyzedSignature])]
type V2N = Vertex -> (AnalyzedTranslation, AnalyzedSignature, [AnalyzedSignature])
type K2V = AnalyzedSignature -> Maybe Vertex

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
findCalls ast expr
  | is _Array   = liftM concat $ mapM (findCalls ast) (expr^?!_Array)
  | is _SConcat = liftM concat $ mapM (findCalls ast) (expr^?!_SConcat)

  | is _Cond    =
      let (c,t,f) = expr^?!_Cond
      in liftM concat $ mapM (findCalls ast) [c,t,f]

  | is (_Funcall._1._Absolute) =
      let (f,args) = expr^?!_Funcall
          absPath  = f^?!_Absolute.from absolute
      in findFunction ast absPath args >>= \tr -> return [tr^.trSig]

  | otherwise = return []
  where
    is prism = has prism expr


isUsed :: (Graph, V2N, K2V) -> AnalyzedTranslation -> Bool
isUsed (g, v2n, k2v) tr =
    case reachable g <$> k2v (tr^.trSig) of
      Nothing -> error $ "isUsed: could not find vertex " ++ show (tr^.trSig)
      Just vs -> any (not . isPrivateTr . view _1 . v2n) vs
