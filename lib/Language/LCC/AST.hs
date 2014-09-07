{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.AST
  ( module Data.TaggedTree
  , module Data.TaggedTree.Flat

  , module Language.LCC.AST.AST
  , module Language.LCC.AST.Annotation
  , module Language.LCC.AST.Expr
  , module Language.LCC.AST.Path
  , module Language.LCC.AST.Scope
  , module Language.LCC.AST.Signature
  , module Language.LCC.AST.Translation

  , LocaleInput(..)
  , inputParam
  , inputAnnotations

  , Locale (..)
  , localeName
  , localeInputs
  , localeAST

  , RelLocale
  , AbsLocale

  , RawLocale
  , AnalyzedLocale

  , Import (..)
  , importPath
  , importName
  ) where

import Control.Lens

import Data.TaggedTree
import Data.TaggedTree.Flat

import Language.LCC.AST.AST
import Language.LCC.AST.Annotation
import Language.LCC.AST.Expr
import Language.LCC.AST.Path
import Language.LCC.AST.Scope
import Language.LCC.AST.Signature
import Language.LCC.AST.Translation



data LocaleInput = LocaleInput { _inputParam       :: Param
                               , _inputAnnotations :: [Annotation]
                               }
  deriving (Eq, Show)

makeLenses ''LocaleInput


data Locale path ret = Locale { _localeName   :: String
                              , _localeInputs :: [LocaleInput]
                              , _localeAST    :: AST path ret
                              }
  deriving (Eq, Show)

makeLenses ''Locale


type RelLocale ret = Locale RelativeVarPath ret
type AbsLocale ret = Locale AbsoluteVarPath ret

type RawLocale = RelLocale UnknownType
type AnalyzedLocale = AbsLocale Type


data Import = Import { _importPath :: PathNode
                     , _importName :: PathNode
                     }
  deriving (Eq, Show)

makeLenses ''Import
