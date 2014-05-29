module Language.LCC.Types.FlatAST where

import Control.Lens

import Data.Function
import Data.Functor
import Data.List (partition, groupBy)
import Data.Maybe
import Data.Monoid

import qualified Data.Map.Strict as Map


import Language.LCC.Types.AST
import Language.LCC.Types.Path


type FlatAST path ret = [(AbsolutePath, [Translation path ret])]
type FlatRelAST ret = FlatAST RelativeVarPath ret
type FlatAbsAST ret = FlatAST AbsoluteVarPath ret

type FlatASTMap path ret = Map.Map AbsolutePath [Translation path ret]
type FlatRelASTMap ret = FlatASTMap RelativeVarPath ret
type FlatAbsASTMap ret = FlatASTMap AbsoluteVarPath ret



paths :: (Ord tag, Monoid s, Cons' s tag) => TaggedTree tag a -> [s]
paths (Leaf _)    = []
paths (Subtree m) = Map.foldrWithKey collect [] m
  where
    collect k (Leaf _) acc = (k <| mempty) : acc
    collect k tree     acc = map (k <|) (paths tree) ++ acc



flatten :: (Ord tag, Monoid s, Cons' s tag) => TaggedTree tag a -> [(s, [a])]
flatten tree = zip (paths tree) (leafs tree)
  where
    leafs :: Ord tag => TaggedTree tag a -> [[a]]
    leafs (Subtree m) = concatMap (leafs . snd) (Map.toList m)
    leafs (Leaf xs) = [xs]



rebuild :: (Ord tag, Cons' s tag)
        => [(s, [a])]
        -> TaggedTree tag a
rebuild ns =
    let (leafs, subtrees) = partition (isSingleElement . fst) ns
                            & _1 %~ groupLeafs
                            & _2 %~ subtreeMap
    in Subtree $ Map.fromList (leafs <> subtrees)
  where
    groupLeafs :: (Eq tag, Cons' s tag) => [(s, [a])] -> [(tag, TaggedTree tag a)]
    groupLeafs = map (_2 %~ Leaf)
               . map (_1 %~ unsafeHead)

    subtreeMap :: (Ord tag, Cons' s tag) => [(s, [a])] -> [(tag, TaggedTree tag a)]
    subtreeMap = map (_2 %~ rebuild)
               . map unsafeExtractHead
               . groupBy ((==) `on` unsafeHead . fst)


flattenOverloads :: [(s, [a])] -> [(s, a)]
flattenOverloads = concatMap (\(s,as) -> (,) s <$> as)


groupOverloads :: Eq s => [(s, a)] -> [(s, [a])]
groupOverloads = map unsafeExtractFst
               . groupBy ((==) `on` fst)


flatOverloads :: Eq s => Iso' [(s, [a])] [(s, a)]
flatOverloads = iso flattenOverloads groupOverloads


unsafeExtractFst :: [(tag, a)] -> (tag, [a])
unsafeExtractFst lfs = lfs & traverse %%~ _1 %~ First . Just
                     & _1 %~ fromJust . getFirst

unsafeExtractHead :: Cons' s tag => [(s,a)] -> (tag, [(s, a)])
unsafeExtractHead sts = sts & traverse._1 %%~ (_1 %~ First . Just) . unsafeUncons
                      & _1 %~ fromJust . getFirst

isSingleElement :: Cons' s tag => s -> Bool
isSingleElement s = hasn't (_tail._head) s


unsafeUncons :: Cons' s a => s -> (a,s)
unsafeUncons s = s^?!_Cons

unsafeHead :: Cons' s a => s -> a
unsafeHead = fst . unsafeUncons

unsafeTail :: Cons' s a => s -> s
unsafeTail = snd . unsafeUncons
