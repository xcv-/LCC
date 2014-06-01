{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.AST.Annotation where

import Control.Lens.TH (makePrisms)


data Annotation
    = PrivateBuiltin
    | Private
    deriving (Eq, Ord, Show)

makePrisms ''Annotation


isBuiltin :: Annotation -> Bool
isBuiltin PrivateBuiltin = True
isBuiltin _              = False

isPrivate :: Annotation -> Bool
isPrivate _ = True
