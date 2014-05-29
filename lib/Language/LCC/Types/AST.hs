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
import Data.Functor
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
    | Leaf [a] -- required for overloading
    deriving (Eq, Show)

makePrisms ''TaggedTree


{-# WARNING leaf "Not an Iso: Just (Subtree [])^.leaf.from leaf = Nothing" #-}
leaf :: Iso' (Maybe (TaggedTree tag a)) [a]
leaf = iso (^.._Just._Leaf.traverse) $ \case
               [] -> Nothing
               xs -> Just (Leaf xs)


_Single :: Prism' [a] a
_Single = prism (:[]) $ \case { [a] -> Right a; t -> Left t }

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



type RelTranslation ret = Translation RelativeVarPath ret
type AbsTranslation ret = Translation AbsoluteVarPath ret
type RawTranslation = RelTranslation UnknownType
type AnalyzedTranslation = AbsTranslation Type


type AST path ret = TaggedTree PathNode (Translation path ret)

type RelAST ret = AST RelativeVarPath ret
type AbsAST ret = AST AbsoluteVarPath ret
type RawAST = RelAST UnknownType
type AnalyzedAST = AbsAST Type



trParamTypes :: Traversal' (Translation path ret) Type
trParamTypes = trSig.sigParamTypes


matchTrParams :: Eq path => Translation path ret -> Translation path ret -> Bool
matchTrParams t1 t2 = matchSig (t1^.trSig) (t2^.trSig)


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
