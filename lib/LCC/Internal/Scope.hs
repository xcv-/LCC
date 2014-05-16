{-# LANGUAGE ConstraintKinds #-}
module LCC.Internal.Scope where

import Data.Applicative

import Control.Monad.Error
import Text.Printf (printf)

import LCC.Internal.Error
import LCC.Internal.Path
import LCC.Internal.AST



data Scope n = Scope { _scopePath :: AbsolutePath
                     , _scopeData :: Maybe n
                     }
    deriving Eq


instance Show n => Show (Scope n) where
    show (Scope path (Just a)) =
        printf "%s(%s)" (show path) (show a)

    show (Scope path Nothing) =
        printf "%s" (show path)


type ScopeT n m a = StateT (Scope n) m a
type Scoped n     = MonadState (Scope n)

type ScopedAST path ret = Scoped (Translation path ret)

runScopeT :: ScopeT n m a -> m a
runScopeT m = evalStateT m topLevel




topLevel :: Scope n
topLevel = Scope mempty Nothing


extendScope :: PathNode -> Scope n -> Scope n
extendScope node = scopePath.absolute |>~ node


inModifiedScope :: Scoped n m => (Scope n -> Scope n) -> m a -> m a
inModifiedScope f m = do
    oldScope <- get
    modify f
    x <- m
    put oldScope
    return x

inScope :: Scoped n m => Scope n -> m a -> m a
inScope scope m = inModifiedScope (const scope) m


scopedTraverse :: (Scoped n m, MonadError LCE.Error m, Applicative m)
               => (a -> m b)
               -> TaggedTree PathNode a
               -> m (TaggedTree PathNode b)
scopedTraverse f tree =
    case tree of
        Leaf x ->
            Leaf <$> inModifiedScope (scopeData .~ Just x) (f x)

        Subtree m -> do
            scope <- liftM (scopeData .~ Nothing) get
            Map.traverseWithKey $ \k v ->
                inScope (extendScope k scope)
                        (scopedTraverse f v)


makeLenses ''Scope
