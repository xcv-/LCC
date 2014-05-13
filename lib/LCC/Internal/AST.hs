import Control.Applicative
import Control.Lens

import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Foldable
import Data.Traversable


data TaggedTree tag a
    = Subtree (Map.Map tag (TaggedTree tag a))
    | Leaf a
    deriving (Eq, Show)

paths :: Ord tag => TaggedTree tag a -> [[tag]]
paths (Leaf _)    = []
paths (Subtree m) = Map.foldrWithKey f [] m
  where
    f :: Ord tag => tag -> TaggedTree tag a -> [[tag]] -> [[tag]]
    f k (Leaf _) acc = [k] : acc
    f k v        acc = map (k:) (paths v) ++ acc

flatten :: Ord tag => TaggedTree tag a -> [([tag], a)]
flatten tree = zip (paths tree) (toList tree)

{-
data Translation path ret =
    Translation { _signature :: Signature path ret
                , _impl :: Expr path
                }


type AST path ret = TaggedTree PathNode (Translation path ret)
    -}


instance Functor (TaggedTree tag) where
    fmap f (Subtree m) = Subtree $ (fmap.fmap) f m
    fmap f (Leaf x)    = Leaf (f x)

instance Foldable (TaggedTree tag) where
    foldMap f (Subtree m) = (foldMap.foldMap) f m
    foldMap f (Leaf x)    = f x

instance Traversable (TaggedTree tag) where
    sequenceA (Subtree m) = Subtree <$> sequenceA (fmap sequenceA m)
    sequenceA (Leaf x)    = Leaf <$> x



type instance Index (TaggedTree tag a)   = tag
type instance IxValue (TaggedTree tag a) = TaggedTree tag a


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
