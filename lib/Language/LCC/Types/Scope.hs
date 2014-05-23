{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.Types.Scope where

import Prelude hiding (mapM, mapM_)

import Control.Applicative
import Control.Lens
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

import Data.Foldable
import Data.Traversable

import qualified Text.Parsec.Pos as Parsec
import Text.Printf (printf)

import Language.LCC.Types.AST
import Language.LCC.Types.Path


newtype ScopeData path ret = ScopeData { _scopeTr :: Translation path ret }
  deriving Eq

makeLenses ''ScopeData

instance (Show path, Show ret) => Show (ScopeData path ret) where
    show d =
        "[" ++ posStr ++ "] " ++ show (d^.scopeTr.trSignature)
      where
        posStr = printf "\"%s\" l%d c%d"
            (Parsec.sourceName   srcPos)
            (Parsec.sourceLine   srcPos)
            (Parsec.sourceColumn srcPos)

        srcPos = d^.scopeTr.trSourcePos



type ScopeT path ret m a = ReaderT (ScopeData path ret) m a
type ScopeRelT ret m a = ScopeT RelativeVarPath ret m a
type ScopeAbsT ret m a = ScopeT AbsoluteVarPath ret m a

type Scoped path ret m = MonadReader (ScopeData path ret) m
type ScopedRel ret m = Scoped RelativeVarPath ret m
type ScopedAbs ret m = Scoped AbsoluteVarPath ret m


runScopeT :: Translation path ret -> ScopeT path ret m a -> m a
runScopeT = flip runReaderT . ScopeData


infixl 0 />
infixl 1 ./>

(/>) :: Translation path ret -> ScopeT path ret m a -> m a
t /> ma = runScopeT t ma

(./>) :: Translation path ret
      -> (Translation path ret -> ScopeT path ret m a)
      -> m a
t ./> f = t /> f t



scopedFoldlM :: Monad m
             => (b -> Translation path ret -> ScopeT path ret m b)
             -> b
             -> AST path ret
             -> m b
scopedFoldlM f = foldlM $ \acc -> (./> f acc)


scopedTraverse :: Applicative f
               => (Translation path ret -> ScopeT path ret f b)
               -> AST path ret
               -> f (TaggedTree PathNode b)
scopedTraverse f = traverse (./> f)


scopedMapM :: Monad m
           => (Translation path ret -> ScopeT path ret m b)
           -> AST path ret
           -> m (TaggedTree PathNode b)
scopedMapM f = mapM (./> f)


scopedMapM_ :: Monad m
            => (Translation path ret -> ScopeT path ret m b)
            -> AST path ret
            -> m ()
scopedMapM_ f = mapM_ (./> f)

