module LCC.Target where

import System.FilePath
import qualified Data.Text as T

import LCC.Types


class Target t where
    setEnv :: t -> LC ()
    output :: t -> [Locale] -> LC [(FilePath, T.Text)]
