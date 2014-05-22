{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.Types.AST where

import Prelude hiding (mapM_, foldl)

import Control.Applicative
import Control.Monad.Reader hiding (mapM_)
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
import Text.Printf (printf)

import Language.LCC.Types.Expr
import Language.LCC.Types.Path
import Language.LCC.Types.Scope
import Language.LCC.Types.Signature



data TaggedTree tag a
    = Subtree (Map.Map tag (TaggedTree tag a))
    | Leaf a
    deriving (Eq, Show)

makePrisms ''TaggedTree


type instance Index (TaggedTree tag a)   = tag
type instance IxValue (TaggedTree tag a) = TaggedTree tag a


instance Functor (TaggedTree tag) where
    fmap f (Subtree m) = Subtree $ (fmap.fmap) f m
    fmap f (Leaf x)    = Leaf (f x)

instance Foldable (TaggedTree tag) where
    foldMap f (Subtree m) = (foldMap.foldMap) f m
    foldMap f (Leaf x)    = f x

instance Traversable (TaggedTree tag) where
    sequenceA (Subtree m) = Subtree <$> sequenceA (fmap sequenceA m)
    sequenceA (Leaf x)    = Leaf <$> x


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



paths :: (Ord tag, Monoid s, Cons s s tag tag) => TaggedTree tag a -> [s]
paths (Leaf _)    = []
paths (Subtree m) = Map.foldrWithKey collect [] m
  where
    collect k (Leaf _) acc = (k <| mempty) : acc
    collect k tree     acc = map (k <|) (paths tree) ++ acc

flatten :: (Ord tag, Monoid s, Cons s s tag tag) => TaggedTree tag a -> [(s, a)]
flatten tree = zip (paths tree) (toList tree)


rebuild :: (Ord tag, Cons s s tag tag)
        => [(s, a)]
        -> TaggedTree tag a
rebuild ns =
    let (leafs, subtrees) = partition (isSingleElement . fst) ns
                            & _1 %~ map (_1 %~ head) . map (_2 %~ Leaf)
                            & _2 %~ subtreeMap
     in Subtree $ Map.fromList (leafs <> subtrees)
  where
    subtreeMap :: (Ord tag, Cons s s tag tag)
               => [(s, a)] -> [(tag, TaggedTree tag a)]
    subtreeMap = map (over _2 rebuild)
               . map extractHead
               . groupBy ((==) `on` head . fst)

    extractHead :: Cons s s tag tag => [(s,a)] -> (tag, [(s, a)])
    extractHead sts = sts & traverse . _1 %%~ over _1 (First . Just) . uncons
                          & _1 %~ fromJust . getFirst

    isSingleElement :: Cons s s tag tag => s -> Bool
    isSingleElement s = hasn't (_tail._head) s

    uncons :: Cons s s a a => s -> (a,s)
    uncons s = s^?!_Cons

    head :: Cons s s a a => s -> a
    head = fst . uncons

    tail :: Cons s s a a => s -> s
    tail = snd . uncons



data Translation path ret =
    Translation { _trSignature :: Signature AbsolutePath ret
                , _trImpl      :: Expr path
                , _trSourcePos :: Parsec.SourcePos
                }
  deriving (Eq)

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

type FlatASTMap path ret = Map.Map AbsolutePath (Translation path ret)
type FlatRelASTMap ret = FlatASTMap RelativeVarPath ret
type FlatAbsASTMap ret = FlatASTMap AbsoluteVarPath ret


newtype TranslationScopeData path ret = TSD (Translation path ret)
  deriving (Eq)

makeIso ''TranslationScopeData

tsd :: Iso' (TranslationScopeData path ret) (Translation path ret)
tsd = from tSD


type ASTScope path ret m = Scope (TranslationScopeData path ret)

type ScopedAST path ret m a = ScopeT (TranslationScopeData path ret) m a
type ScopedRelAST ret m a = ScopedAST RelativeVarPath ret m a
type ScopedAbsAST ret m a = ScopedAST AbsoluteVarPath ret m a


infixl 0 />
infixl 1 ./>

(/>) :: Translation path ret
     -> ScopedAST path ret m a
     -> m a
t /> ma = runReaderT ma (Scope $ TSD t)

(./>) :: Translation path ret
      -> (Translation path ret -> ScopedAST path ret m a)
      -> m a
t ./> f = t /> f t



scopedTraverse :: (Applicative m, Monad m)
               => (Translation path ret -> ScopedAST path ret m b)
               -> AST path ret
               -> m (TaggedTree PathNode b)
scopedTraverse f = traverse (./> f)


scopedFoldlM :: Monad m
             => (b -> Translation path ret -> ScopedAST path ret m b)
             -> b
             -> AST path ret
             -> m b
scopedFoldlM f = foldlM $ \acc -> (./> f acc)


scopedMapM_ :: Monad m
            => (Translation path ret -> ScopedAST path ret m b)
            -> AST path ret
            -> m ()
scopedMapM_ f = mapM_ (./> f)



atPath :: AbsolutePath -> Traversal' (AST path ret) (Translation path ret)
atPath path =
    case Seq.viewl (path^.absolute) of
      EmptyL  -> ignored

      n :< ns -> foldl' (\acc node -> acc . _Just . at node) (at n) ns . _Just . _Leaf


lookupParam :: Translation path ret -> PathNode -> Maybe Param
lookupParam t name =
    t^?trSignature.sigParams.folded.filtered (\p -> p^.paramName == name)


instance (Show path, Show ret) => Show (TranslationScopeData path ret) where
    show d =
        "[" ++ posStr ++ "] " ++ show (d^.tsd.trSignature)
      where
        posStr = printf "\"%s\" l%d c%d"
            (Parsec.sourceName   srcPos)
            (Parsec.sourceLine   srcPos)
            (Parsec.sourceColumn srcPos)

        srcPos = d^.tsd.trSourcePos
