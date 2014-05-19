{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.Types.Scope where

import Control.Lens
import Control.Monad.Reader


newtype Scope n = Scope { _scopeData :: n }
  deriving Eq

makeLenses ''Scope


instance Show n => Show (Scope n) where
    show (Scope sd) = show sd


type ScopeT n m a = ReaderT (Scope n) m a
type Scoped n m   = MonadReader (Scope n) m
