{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.Types
  ( module Language.LCC.Types.AST
  --, module Language.LCC.Types.Environment
  , module Language.LCC.Types.Expr
  , module Language.LCC.Types.Path
  , module Language.LCC.Types.Scope
  , module Language.LCC.Types.Signature

  , Locale (..)
  , localeName
  , localeAST

  , RelLocale
  , AbsLocale

  , RawLocale
  , AnalyzedLocale
  ) where

import Control.Lens

import Language.LCC.Types.AST
--import Language.LCC.Types.Environment
import Language.LCC.Types.Expr
import Language.LCC.Types.Path
import Language.LCC.Types.Scope
import Language.LCC.Types.Signature


data Locale path ret = Locale { _localeName :: String
                              , _localeAST  :: AST path ret
                              }

type RelLocale ret = Locale RelativeVarPath ret
type AbsLocale ret = Locale AbsoluteVarPath ret

type RawLocale = RelLocale UnknownType
type AnalyzedLocale = AbsLocale Type

makeLenses ''Locale
