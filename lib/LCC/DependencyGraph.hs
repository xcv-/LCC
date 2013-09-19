module LCC.DependencyGraph
  ( dependencyGraph
  , typeInferenceDependencies
  , sortGraph
  ) where


import Control.Monad

import Data.Function
import Data.Functor
import Data.List

import LCC.Internal.Types
import qualified Data.Graph as Graph
import qualified Data.Set as Set


dependencyGraph :: Ord key =>

                   [(key, node)]

                -> (node -> [key])

                -> ( Graph.Graph
                   , Graph.Vertex -> (node, key, [key])
                   , key -> Maybe Graph.Vertex
                   )
dependencyGraph exprs listKeys = Graph.graphFromEdges $ map adapt exprs
  where
    adapt (path, expr) = (expr, path, listKeys expr)


typeInferenceDependencies :: Set.Set AbsVarPath -> Expr -> [AbsVarPath]
typeInferenceDependencies known (ArrayLiteral exprs) =
    minimumBy (compare `on` length) $ map (typeInferenceDependencies known) exprs

typeInferenceDependencies known (Funcall (AbsolutePath path) _)
  | path' `Set.member` known = []
  | otherwise = [path']
  where
    path' = AbsVarPath path

typeInferenceDependencies _ _ = []


sortGraph :: Graph.Graph -> [Graph.Vertex]
sortGraph = reverse . Graph.topSort
