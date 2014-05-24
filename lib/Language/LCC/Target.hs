{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Target where

import Data.String

import System.FilePath

import qualified Language.LCC.Error as Err
import Language.LCC.Types


class Target t where
    injectBuiltins :: Err.ErrorM m
                   => t
                   -> Locale path ret
                   -> m (Locale path ret)

    runBuiltin :: (Err.ErrorM m, ScopedAbs Type m)
               => t
               -> AbsolutePath
               -> [AbsExpr]
               -> m (Maybe AbsExpr)

    output :: IsString s
           => t
           -> [AnalyzedLocale]
           -> [(FilePath, s)]
