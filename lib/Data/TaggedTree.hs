{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Data.TaggedTree where

import Prelude hiding (mapM_, foldl)

import GHC.Exts (IsList, Item, toList)

import Control.Applicative
import Control.Lens
import Control.Monad (liftM2)

import Data.Monoid
import Data.Foldable hiding (toList, for_)
import Data.Traversable

import qualified Data.Map.Strict as Map


data TaggedTree tag a
    = Subtree (Map.Map tag (TaggedTree tag a))
    | Leaf [a] -- required for overloading
    deriving (Eq, Show)

makePrisms ''TaggedTree


type Cons' s a = Cons s s a a

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
    ix _ _ tree@(Leaf _) = pure tree
    ix k f tree@(Subtree m) = case Map.lookup k m of
        Nothing -> pure tree
        Just v  -> (\v' -> Subtree $ Map.insert k v' m) <$> f v

instance Ord tag => At (TaggedTree tag a) where
    at k = lens getter setter
      where
        getter (Subtree m) = Map.lookup k m
        getter (Leaf _) = Nothing

        setter tree@(Leaf _) _  = tree
        setter (Subtree m) mv   = case mv of
            Nothing -> Subtree (Map.delete k m)
            Just v  -> Subtree (Map.insert k v m)


emptyTree :: Ord tag => TaggedTree tag a
emptyTree = Subtree Map.empty


filterTree :: Ord tag
           => (a -> Bool)
           -> TaggedTree tag a
           -> TaggedTree tag a
filterTree p tree =
    case tree of
      Leaf xs   -> Leaf (filter p xs)
      Subtree m -> let m' = fmap (filterTree p) m
                   in Subtree $ Map.filter (not . isEmpty) m'
  where
    isEmpty (Subtree m) = Map.null m
    isEmpty (Leaf [])   = True
    isEmpty _           = False


foldMapWithTags :: (Ord tag, Monoid acc)
                => Map.Map tag (TaggedTree tag a)
                -> (tag -> a -> acc)
                -> (tag -> Map.Map tag (TaggedTree tag a) -> acc)
                -> acc
foldMapWithTags m fLeaf fSubtree =
    Map.foldMapWithKey f m
  where
    f tag (Leaf as)    = foldMap (fLeaf tag) as
    f tag (Subtree m') = fSubtree tag m'


newtype MMonoid m n = MMonoid { unwrap :: m n }

instance (Monad m, Monoid n) => Monoid (MMonoid m n) where
  mempty      = MMonoid $ return mempty
  mappend a b = MMonoid $ liftM2 mappend (unwrap a) (unwrap b)


foldMapWithTagsM :: (Monad m, Ord tag, Monoid acc)
                 => Map.Map tag (TaggedTree tag a)
                 -> (tag -> a -> m acc)
                 -> (tag -> Map.Map tag (TaggedTree tag a) -> m acc)
                 -> m acc
foldMapWithTagsM m fLeaf fSubtree =
    unwrap $
      foldMapWithTags m (\tag -> MMonoid . fLeaf tag)
                        (\tag -> MMonoid . fSubtree tag)


mapWithTagsM_ :: (Monad m, Ord tag)
              => Map.Map tag (TaggedTree tag a)
              -> (tag -> a -> m b)
              -> (tag -> Map.Map tag (TaggedTree tag a) -> m c)
              -> m ()
mapWithTagsM_ m fLeaf fSubtree =
    iforM_ m $ \tag subtree ->
      case subtree of
        Leaf as    -> mapM_ (fLeaf tag) as
        Subtree m' -> fSubtree tag m' >> return ()


-- Custom lenses and prisms

_Single :: Prism' [a] a
_Single = prism (:[]) $ \case { [a] -> Right a; t -> Left t }

_SingleLeaf :: Prism' (TaggedTree tag a) a
_SingleLeaf = _Leaf._Single


atPath :: (IsList path, (Item path) ~ tag, Ord tag)
       => path
       -> Lens' (TaggedTree tag a) (Maybe (TaggedTree tag a))
atPath path =
    case uncons (toList path) of
      Nothing     -> lens (const Nothing) const
      Just (n,ns) -> foldl' (\acc node -> acc.non' _Empty.at node) (at n) ns
  where
    _Empty = prism' (const emptyTree) $ \case
        Leaf _ -> Nothing
        Subtree m
          | Map.null m -> Just ()
          | otherwise  -> Nothing
