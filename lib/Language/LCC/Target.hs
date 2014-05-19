{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Target where

import System.FilePath
import qualified Data.Text as T

import qualified Language.LCC.Error as Err
import Language.LCC.Types


class Target t where
    injectBuiltins :: (Err.ErrorM m)
                   => t
                   -> Locale path ret
                   -> m (Locale path ret)

    output :: t -> [AnalyzedLocale] -> IO [(FilePath, T.Text)]
