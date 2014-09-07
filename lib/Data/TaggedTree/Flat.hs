module Data.TaggedTree.Flat where

import Control.Lens

import Data.Function
import Data.Functor
import Data.List (partition, groupBy)
import Data.Monoid

import qualified Data.Map.Strict as Map

import Data.TaggedTree


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
               . groupByFstHead


flattenOverloads :: [(s, [a])] -> [(s, a)]
flattenOverloads = concatMap (\(s,as) -> (,) s <$> as)


groupOverloads :: Eq s => [(s, a)] -> [(s, [a])]
groupOverloads = groupByFst


flatOverloads :: Eq s => Iso' [(s, [a])] [(s, a)]
flatOverloads = iso flattenOverloads groupOverloads


groupByFst :: Eq a => [(a, b)] -> [(a, [b])]
groupByFst =
    map unsafeExtractFst . groupBy ((==) `on` fst)

groupByFstHead :: (Eq a, Cons' s a) => [(s, b)] -> [(a, [(s, b)])]
groupByFstHead =
    map unsafeExtractHead . groupBy ((==) `on` unsafeHead . fst)


unsafeExtractFst :: [(tag, a)] -> (tag, [a])
unsafeExtractFst [] = error "unsafeExtractFst: empty list"
unsafeExtractFst lfs@((tag,_):_) =
    (tag, map snd lfs)

unsafeExtractHead :: Cons' s tag => [(s,a)] -> (tag, [(s,a)])
unsafeExtractHead [] = error "unsafeExtractHead: empty list"
unsafeExtractHead sts@((s,_):_) =
    (unsafeHead s, map (_1 %~ unsafeTail) sts)

isSingleElement :: Cons' s tag => s -> Bool
isSingleElement = hasn't (_tail._head)


unsafeUncons :: Cons' s a => s -> (a,s)
unsafeUncons s = s^?!_Cons

unsafeHead :: Cons' s a => s -> a
unsafeHead = fst . unsafeUncons

unsafeTail :: Cons' s a => s -> s
unsafeTail = snd . unsafeUncons
