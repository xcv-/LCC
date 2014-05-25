{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Target where

import Control.Applicative

import qualified Data.Text.Lazy as T

import System.FilePath

import qualified Language.LCC.Error as Err
import Language.LCC.Types


class Target t where
    injectBuiltins :: (Err.ErrorM m, Applicative m)
                   => t
                   -> AbsLocale UnknownType
                   -> m (AbsLocale UnknownType)

    output :: Err.ErrorM m
           => t
           -> [AnalyzedLocale]
           -> m [(FilePath, T.Text)]
