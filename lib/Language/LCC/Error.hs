{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Language.LCC.Error where

import Control.Lens
import qualified Control.Monad.Except as ME

import Data.Function (on)
import Data.List

import Text.Parsec
import Text.Printf (printf)

import Language.LCC.AST


newtype Scope path ret = Scope { _scopeTr :: Translation path ret }

makeLenses ''Scope


data MissingSignature = MissingSignature
    { missingInLocale   :: String
    , missingSig        :: AnalyzedSignature
    , missingComparedTo :: String
    }


type ErrorM m = ME.MonadError Error m

data Error where

  Parse              :: { parseError :: ParseError }
                     -> Error

  RelativePath       :: forall path ret. (Show path, Show ret)
                     => { scope :: Scope path ret
                        , path  :: RelativeVarPath
                        }
                     -> Error

  Type               :: forall path ret. (Show path, Show ret)
                     => { scope    :: Scope path ret
                        , expected :: Type
                        , found    :: Type
                        }
                     -> Error

  SymbolNotFound     :: forall path ret. (Show path, Show ret)
                     => { scope       :: Scope path ret
                        , missingPath :: AbsoluteVarPath
                        }
                     -> Error

  SignatureNotFound  :: forall path ret. (Show path, Show ret)
                     => { scope       :: Scope path ret
                        , missingPath :: AbsoluteVarPath
                        , argTypes    :: [Maybe Type]
                        }
                     -> Error

  AmbiguousOverloads :: forall path ret. (Show path, Show ret)
                     => { scope         :: Scope path ret
                        , ambiguousPath :: AbsoluteVarPath
                        , argTypes      :: [Maybe Type]
                        , candidates    :: [AnalyzedTranslation]
                        }
                     -> Error


  SignatureConflict  :: forall ret. Show ret
                     => { conflicting :: [AbsTranslation ret] }
                     -> Error

  Cycle              :: { cyclicSigs :: [AbsTranslation UnknownType] }
                     -> Error

  MissingSignatures  :: { missingSigs :: [MissingSignature] }
                     -> Error

  ScopedPanic        :: forall path ret. (Show path, Show ret)
                     => { scope        :: Scope path ret
                        , panicMessage :: String
                        }
                     -> Error

  Panic              :: { panicMessage :: String }
                     -> Error


instance (Show path, Show ret) => Show (Scope path ret) where
    show s =
        printf "[%s] %s" (posStr :: String) (sigStr :: String)
      where
        sigStr = show $ s^.scopeTr.trSig

        posStr = printf "\"%s\" l%d c%d"
            (sourceName srcPos) (sourceLine srcPos) (sourceColumn srcPos)

        srcPos = s^.scopeTr.trSourcePos


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
          (show scope) (show missingPath) (showArgTypes argTypes)

  show AmbiguousOverloads {..} =
      intercalate "\n" $
          printf "In %s: Matched multiple overloads for %s(%s):"
            (show scope) (show ambiguousPath) (showArgTypes argTypes)
              : map (show . Scope) candidates

  show SignatureConflict {..} =
      intercalate "\n" $
          "Found conflicting signatures:" : map (show . Scope) conflicting

  show Cycle {..} =
      intercalate "\n" $
          "Could not infer return types:" : map (show . Scope) cyclicSigs

  show MissingSignatures {..} =
      let formatted = missingSigs
                    & map (\MissingSignature {..} ->
                            ((missingComparedTo, missingInLocale), missingSig))
                    & sortBy (compare `on` fst)
                    & groupByFst
                    & traverse._2 %~ concatMap ("\n\t"++) . map show
                    :: [((String, String), String)]

          groupHdr = printf "Found in %s, but missing in %s:"

      in intercalate "\n" $
           "Found missing signatures:\n" :
             map (\((f,l),list) -> groupHdr f l ++ list) formatted


  show ScopedPanic {..} =
      printf "In %s: PANIC: %s" (show scope) panicMessage

  show Panic {..} =
      printf "PANIC: %s" panicMessage


showArgTypes :: [Maybe Type] -> String
showArgTypes = intercalate ", " . map (maybe "?" show)



-- Helpers

rethrow :: ErrorM m => Error -> m a
rethrow = ME.throwError

catching :: ErrorM m => m a -> (Error -> m a) -> m a
catching = ME.catchError


parsingError :: ErrorM m => ParseError -> m a
parsingError = ME.throwError . Parse


invalidRelativePath :: (ErrorM m, Scoped path ret m, Show path, Show ret)
                    => RelativeVarPath -> m a
invalidRelativePath path = do
    scope <- viewS (to Scope)
    ME.throwError RelativePath {..}


typeError :: (ErrorM m, Scoped path ret m, Show path, Show ret)
          => Type -> Type -> m a
typeError expected found = do
    scope <- viewS (to Scope)
    ME.throwError Type {..}


symbolNotFound :: (ErrorM m, Scoped path ret m, Show path, Show ret)
               => AbsoluteVarPath -> m a
symbolNotFound missingPath = do
    scope <- viewS (to Scope)
    ME.throwError SymbolNotFound {..}


signatureNotMatched :: (ErrorM m, Scoped path ret m, Show path, Show ret)
                  => AbsoluteVarPath -> [Maybe Type] -> m a
signatureNotMatched missingPath argTypes = do
    scope <- viewS (to Scope)
    ME.throwError SignatureNotFound {..}


signatureNotFound :: (ErrorM m, Scoped path ret m, Show path, Show ret)
                  => AbsoluteVarPath -> [Type] -> m a
signatureNotFound missingPath argTypes =
    signatureNotMatched missingPath (map Just argTypes)


ambiguousOverloads :: (ErrorM m, Scoped path ret m, Show path, Show ret)
                   => AbsoluteVarPath
                   -> [Maybe Type]
                   -> [AnalyzedTranslation]
                   -> m a
ambiguousOverloads ambiguousPath argTypes candidates = do
    scope <- viewS (to Scope)
    ME.throwError AmbiguousOverloads {..}


cycle :: ErrorM m => [AbsTranslation UnknownType] -> m a
cycle = ME.throwError . Cycle


conflict :: (ErrorM m, Show ret) => [AbsTranslation ret] -> m a
conflict = ME.throwError . SignatureConflict


missingSignatures :: ErrorM m => [MissingSignature] -> m a
missingSignatures = ME.throwError . MissingSignatures


panic :: (ErrorM m, Scoped path ret m, Show path, Show ret)
      => String -> m a
panic panicMessage = do
    scope <- viewS (to Scope)
    ME.throwError ScopedPanic {..}


globalPanic :: ErrorM m => String -> m a
globalPanic panicMessage =
    ME.throwError Panic {..}
