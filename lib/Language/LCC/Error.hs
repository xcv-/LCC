{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Language.LCC.Error where

import Control.Lens
import qualified Control.Monad.Error as ME
import Control.Monad.Reader

import Data.Functor
import Data.List

import qualified Text.Parsec as Parsec
import Text.Printf (printf)

import Language.LCC.Types


type ErrorM m = ME.MonadError Error m


data Error where

  Parse             :: { parseError :: Parsec.ParseError }
                    -> Error

  RelativePath      :: forall path ret. (Show path, Show ret)
                    => { scope :: ScopeData path ret
                       , path  :: RelativeVarPath
                       }
                    -> Error

  Type              :: forall path ret. (Show path, Show ret)
                    => { scope    :: ScopeData path ret
                       , expected :: Type
                       , found    :: Type
                       }
                    -> Error

  SymbolNotFound    :: forall path ret. (Show path, Show ret)
                    => { scope       :: ScopeData path ret
                       , missingPath :: AbsoluteVarPath
                       }
                    -> Error

  SignatureNotFound :: forall path ret. (Show path, Show ret)
                    => { scope       :: ScopeData path ret
                       , missingPath :: AbsoluteVarPath
                       , argTypes    :: [Type]
                       }
                    -> Error

  SignatureConflict :: { conflicting :: [AbsTranslation Type] }
                    -> Error

  Cycle             :: { cyclicSigs :: [AbsTranslation UnknownType] }
                    -> Error

  Interface         :: { localeName  :: String
                       , missingSigs :: [Signature AbsolutePath Type]
                       }
                    -> Error

  ScopedPanic       :: forall path ret. (Show path, Show ret)
                    => { scope        :: ScopeData path ret
                       , panicMessage :: String
                       }
                    -> Error

  Panic             :: { panicMessage :: String }
                    -> Error



instance Show Error where
  show Parse {..} =
      show parseError

  show RelativePath {..} =
      printf "In %s: Invalid relative path %s"
          (show scope) (show path)

  show Type {..} =
      printf "In %s: Type error: Expected type '%s' but found '%s'"
          (show scope) (show expected) (show found)

  show SymbolNotFound {..} =
      printf "In %s: Symbol not found: %s"
          (show scope) (show missingPath)

  show SignatureNotFound {..} =
      printf "In %s: Signature not found: %s(%s)"
          (show scope) (show missingPath) (intercalate ", " $ map show argTypes)

  show SignatureConflict {..} =
      intercalate "\n" $
          "Found conflicting signatures:" : map (show . view trSignature) conflicting

  show Cycle {..} =
      intercalate "\n" $
          "Dependency cycle found:" : map (show . view trSignature) cyclicSigs

  show Interface {..} =
      intercalate "\n" $
          ("Missing signatures in " ++ show localeName) : map show missingSigs

  show ScopedPanic {..} = printf "In %s: PANIC: %s" (show scope) panicMessage

  show Panic {..} = printf "PANIC: %s" panicMessage




invalidRelativePath :: (ErrorM m, Scoped path ret m, Show path, Show ret)
                    => RelativeVarPath -> m a
invalidRelativePath path = do
    scope <- ask
    ME.throwError RelativePath {..}



typeError :: (ErrorM m, Scoped path ret m, Show path, Show ret)
          => Type -> Type -> m a
typeError expected found = do
    scope <- ask
    ME.throwError Type {..}



symbolNotFound :: (ErrorM m, Scoped path ret m, Show path, Show ret)
               => AbsoluteVarPath -> m a
symbolNotFound missingPath = do
    scope <- ask
    ME.throwError SymbolNotFound {..}



signatureNotFound :: (ErrorM m, Scoped path ret m, Show path, Show ret)
                  => AbsoluteVarPath -> [Type] -> m a
signatureNotFound missingPath argTypes = do
    scope <- ask
    ME.throwError SignatureNotFound {..}


cycle :: ErrorM m => [AbsTranslation UnknownType] -> m a
cycle = ME.throwError . Cycle


panic :: (ErrorM m, Scoped path ret m, Show path, Show ret)
      => String -> m a
panic panicMessage = do
    scope <- ask
    ME.throwError ScopedPanic {..}


globalPanic :: ErrorM m => String -> m a
globalPanic panicMessage =
    ME.throwError Panic {..}
