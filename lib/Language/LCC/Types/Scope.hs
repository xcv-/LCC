{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.Types.Scope where

import Prelude hiding (mapM, mapM_)

import Control.Applicative
import Control.Lens

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Either as E
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S

import Control.Monad.Reader hiding (mapM, mapM_)
import Control.Monad.Writer hiding (mapM, mapM_)
import Control.Monad.State hiding (mapM, mapM_)
import Control.Monad.Error hiding (mapM, mapM_)

import Data.Foldable
import Data.Traversable

import Text.Parsec.Pos
import Text.Printf (printf)

import Language.LCC.Types.AST
import Language.LCC.Types.Path



class Monad m => Scoped path ret m | m -> path, m -> ret where
    getS   :: m (Translation path ret)
    localS :: (Translation path ret -> Translation path ret) -> m a -> m a


type ScopedRel ret m = Scoped RelativeVarPath ret m
type ScopedAbs ret m = Scoped AbsoluteVarPath ret m

viewS :: Scoped path ret m => Getting a (Translation path ret) a -> m a
viewS l = liftM (^.l) getS

previewS :: Scoped path ret m
         => Getting (First a) (Translation path ret) a -> m (Maybe a)
previewS l = liftM (^?l) getS



newtype ScopeT path ret m a = ScopeT (Translation path ret -> m a)

type ScopeRelT ret m a = ScopeT RelativeVarPath ret m a
type ScopeAbsT ret m a = ScopeT AbsoluteVarPath ret m a


infixl 0 />
infixl 1 ./>

runScopeT, (/>) :: Translation path ret -> ScopeT path ret m a -> m a
runScopeT t (ScopeT m) = m t

mapScopeT :: (m a -> n b) -> ScopeT path ret m a -> ScopeT path ret n b
mapScopeT f (ScopeT m) = ScopeT $ f . m

(/>) = runScopeT

(./>) :: Translation path ret
      -> (Translation path ret -> ScopeT path ret m a)
      -> m a
t ./> f = t /> f t



instance Functor m => Functor (ScopeT path ret m) where
    fmap f (ScopeT m) = ScopeT (fmap f . m)

instance Applicative m => Applicative (ScopeT path ret m) where
    pure      = ScopeT . const . pure
    mf <*> mx = ScopeT $ \t -> runScopeT t mf <*> runScopeT t mx

instance Monad m => Monad (ScopeT path ret m) where
    return  = ScopeT . const . return
    m >>= f = ScopeT $ \t -> runScopeT t m >>= runScopeT t . f

instance MonadTrans (ScopeT path ret) where
    lift m = ScopeT (const m)

instance Monad m => Scoped path ret (ScopeT path ret m) where
    getS       = ScopeT $ return
    localS f m = ScopeT $ \t -> runScopeT (f t) m



-- MTL Boilerplate instances

instance Scoped path ret m => Scoped path ret (E.EitherT e m) where
    getS   = lift getS
    localS = E.mapEitherT . localS

instance Scoped path ret m => Scoped path ret (R.ReaderT r m) where
    getS   = lift getS
    localS = R.mapReaderT . localS

instance Scoped path ret m => Scoped path ret (L.StateT s m) where
    getS   = lift getS
    localS = L.mapStateT . localS

instance Scoped path ret m => Scoped path ret (S.StateT s m) where
    getS   = lift getS
    localS = S.mapStateT . localS

instance (Monoid w, Scoped path ret m) => Scoped path ret (L.WriterT w m) where
    getS   = lift getS
    localS = L.mapWriterT . localS

instance (Monoid w, Scoped path ret m) => Scoped path ret (S.WriterT w m) where
    getS   = lift getS
    localS = S.mapWriterT . localS



instance MonadError e m => MonadError e (ScopeT path ret m) where
    throwError = lift . throwError
    (ScopeT m) `catchError` f = ScopeT $ \t -> m t `catchError` (runScopeT t . f)

instance MonadReader r m => MonadReader r (ScopeT path ret m) where
    ask    = lift ask
    local  = mapScopeT . local
    reader = lift . reader

instance MonadState r m => MonadState r (ScopeT path ret m) where
    get   = lift get
    put   = lift . put
    state = lift . state

instance (Monoid w, MonadWriter w m) => MonadWriter w (ScopeT path ret m) where
    writer = lift . writer
    tell   = lift . tell
    listen = mapScopeT listen
    pass   = mapScopeT pass




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
