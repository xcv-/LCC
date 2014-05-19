module LCC.State
  ( LocaleState

  , emptyState

  , topLevelScope
  , scopePath
  , getScope
  , getScopePath
  , inScope
  , inInnerScope

  , emptyEnv
  , getEnv
  , modifyEnv
  , addEnv
  , addAllEnv

  , filterParams

  , findGlobalSignatures
  , findGlobalSignature

  , findSignatures
  , findSignature
  ) where


import Data.List
import Data.Functor ((<$>))

import Control.Monad.Error
import Control.Monad.State

import qualified Data.Set as Set

import LCC.Internal.Types



emptyState :: LocaleState
emptyState = LocaleState { lcsScope = topLevelScope
                         , lcsEnv = emptyEnv
                         }


-- Environment

emptyEnv :: Env
emptyEnv = Env Set.empty


getEnv :: MonadState LocaleState m => m Env
getEnv = lcsEnv <$> get

modifyEnv :: MonadState LocaleState m=> (Env -> Env) -> m ()
modifyEnv f = modify $ \ls -> ls { lcsEnv = f (lcsEnv ls) }

addEnv :: MonadState LocaleState m => TranslationSignature -> m ()
addEnv sig = modifyEnv $ Env . Set.insert sig . envSignatures

addAllEnv :: MonadState LocaleState m => [TranslationSignature] -> m ()
addAllEnv = mapM_ addEnv
