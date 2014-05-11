{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module LCC.Internal.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Data.Function
import Data.List
import qualified Data.Set as Set

import Text.Parsec.Error (ParseError)


newtype LC a = LC (StateT LocaleState (Either LocaleError) a)
    deriving ( Functor, Applicative, Monad
             , MonadState LocaleState
             , MonadError LocaleError
             )

runLocale :: LocaleState -> LC a -> Either LocaleError (a, LocaleState)
runLocale st (LC x) = runIdentity . runErrorT $ runStateT x st

