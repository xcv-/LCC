{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.Types.AST where

import Prelude hiding (mapM_, foldl)

import Control.Applicative
import Control.Lens

import Data.Foldable
import Data.Function
import Data.Functor
import Data.List (partition, groupBy)
import Data.Maybe
import Data.Monoid
import Data.Sequence (ViewL(..), ViewR(..))
import Data.Traversable

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Text.Parsec.Pos as Parsec

import Language.LCC.Types.Expr
import Language.LCC.Types.Path
import Language.LCC.Types.Signature



data TaggedTree tag a
    = Subtree (Map.Map tag (TaggedTree tag a))
    | Leaf [a]
    deriving (Eq, Show)

makePrisms ''TaggedTree


_Single :: Prism' [a] a
_Single = prism (:[]) $ \case { [a] -> Right a; t -> Left t }

{-# WARNING leaf "Not really an Iso" #-}
leaf :: Iso' (Maybe (TaggedTree tag a)) [a]
leaf = iso (^.._Just._Leaf.traverse) $ \case
               [] -> Nothing
               xs -> Just (Leaf xs)


_SingleLeaf :: Prism' (TaggedTree tag a) a
_SingleLeaf = _Leaf._Single



type instance Index (TaggedTree tag a)   = tag
type instance IxValue (TaggedTree tag a) = TaggedTree tag a


instance Functor (TaggedTree tag) where
    fmap f (Subtree m) = Subtree $ (fmap.fmap) f m
    fmap f (Leaf x)    = Leaf (fmap f x)

instance Foldable (TaggedTree tag) where
    foldMap f (Subtree m) = (foldMap.foldMap) f m
    foldMap f (Leaf x)    = foldMap f x

instance Traversable (TaggedTree tag) where
    sequenceA (Subtree m) = Subtree <$> sequenceA (fmap sequenceA m)
    sequenceA (Leaf x)    = Leaf <$> sequenceA x


instance Ord tag => Ixed (TaggedTree tag a) where
    ix k f tree@(Leaf _) = pure tree
    ix k f tree@(Subtree m) = case Map.lookup k m of
        Nothing -> pure tree
        Just v  -> (\v' -> Subtree $ Map.insert k v' m) <$> f v

instance Ord tag => At (TaggedTree tag a) where
    at k = lens get set
      where
        get (Subtree m) = Map.lookup k m
        get (Leaf _) = Nothing

        set tree@(Leaf _) _  = tree
        set (Subtree m) mv = case mv of
            Nothing -> Subtree (Map.delete k m)
            Just v  -> Subtree (Map.insert k v m)


emptyTree :: Ord tag => TaggedTree tag a
emptyTree = Subtree Map.empty


paths :: (Ord tag, Monoid s, Cons' s tag) => TaggedTree tag a -> [s]
paths (Leaf _)    = []
paths (Subtree m) = Map.foldrWithKey collect [] m
  where
    collect k (Leaf x) acc = replicate (length x) k : acc
    collect k tree     acc = map (k <|) (paths tree) ++ acc

    replicate :: (Monoid s, Cons' s a) => Int -> a -> s
    replicate n k = foldl' (\acc _ -> k <| acc) mempty [1..n]


flatten :: (Ord tag, Monoid s, Cons' s tag) => TaggedTree tag a -> [(s, a)]
flatten tree = zip (paths tree) (toList tree)


rebuild :: (Ord tag, Cons' s tag)
        => [(s, a)]
        -> TaggedTree tag a
rebuild ns =
    let (leafs, subtrees) = partition (isSingleElement . fst) ns
                            & _1 %~ groupLeafs
                            & _2 %~ subtreeMap
    in Subtree $ Map.fromList (leafs <> subtrees)
  where
    groupLeafs :: (Eq tag, Cons' s tag) => [(s, a)] -> [(tag, TaggedTree tag a)]
    groupLeafs = map (_2 %~ Leaf)
               . map extractFst
               . groupBy ((==) `on` fst)
               . map (_1 %~ unsafeHead)

    subtreeMap :: (Ord tag, Cons' s tag) => [(s, a)] -> [(tag, TaggedTree tag a)]
    subtreeMap = map (_2 %~ rebuild)
               . map extractHead
               . groupBy ((==) `on` unsafeHead . fst)

    extractFst :: [(tag, a)] -> (tag, [a])
    extractFst lfs = lfs & traverse %%~ _1 %~ First . Just
                         & _1 %~ fromJust . getFirst

    extractHead :: Cons' s tag => [(s,a)] -> (tag, [(s, a)])
    extractHead sts = sts & traverse._1 %%~ (_1 %~ First . Just) . unsafeUncons
                          & _1 %~ fromJust . getFirst

    isSingleElement :: Cons' s tag => s -> Bool
    isSingleElement s = hasn't (_tail._head) s

    unsafeUncons :: Cons' s a => s -> (a,s)
    unsafeUncons s = s^?!_Cons

    unsafeHead :: Cons' s a => s -> a
    unsafeHead = fst . unsafeUncons

    unsafeTail :: Cons' s a => s -> s
    unsafeTail = snd . unsafeUncons



mapWithTagsM_ :: (Monad m, Ord tag)
              => Map.Map tag (TaggedTree tag a)
              -> (tag -> a -> m b)
              -> (tag -> Map.Map tag (TaggedTree tag a) -> m c)
              -> m ()
mapWithTagsM_ m fLeaf fSubtree = do
    let for_ m f = Map.foldlWithKey f (return ()) m

    for_ m $ \_ tag subtree ->
      case subtree of
        Leaf as    -> mapM_ (fLeaf tag) as
        Subtree m' -> fSubtree tag m' >> return ()




data Translation path ret =
    Translation { _trSig       :: Signature AbsolutePath ret
                , _trImpl      :: Expr path
                , _trSourcePos :: Parsec.SourcePos
                }
  deriving (Eq, Show)

makeLenses ''Translation


type RelExpr = Expr RelativeVarPath
type AbsExpr = Expr AbsoluteVarPath

type RelTranslation ret = Translation RelativeVarPath ret
type AbsTranslation ret = Translation AbsoluteVarPath ret

type AST path ret = TaggedTree PathNode (Translation path ret)
type RelAST ret = AST RelativeVarPath ret
type AbsAST ret = AST AbsoluteVarPath ret

type FlatAST path ret = [(AbsolutePath, Translation path ret)]
type FlatRelAST ret = FlatAST RelativeVarPath ret
type FlatAbsAST ret = FlatAST AbsoluteVarPath ret

type FlatASTMap path ret = Map.Map AbsolutePath [Translation path ret]
type FlatRelASTMap ret = FlatASTMap RelativeVarPath ret
type FlatAbsASTMap ret = FlatASTMap AbsoluteVarPath ret



atPath :: AbsolutePath -> Lens' (AST path ret) (Maybe (AST path ret))
atPath path =
    case Seq.viewl (path^.absolute) of
      EmptyL  -> lens (const Nothing) const

      n :< ns -> foldl' (\acc node -> acc.non' _Empty.at node) (at n) ns
  where
    _Empty = prism' (const emptyTree) $ \case
        Leaf _ -> Nothing
        Subtree m
          | Map.null m -> Just ()
          | otherwise  -> Nothing


lookupParam :: Translation path ret -> PathNode -> Maybe Param
lookupParam t name =
    t^?trSig.sigParams.folded.filtered (\p -> p^.paramName == name)
