{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.AST.Annotation where

import Control.Lens.TH (makePrisms)


data Annotation
    = PrivateBuiltin
    | Private
    deriving (Eq, Ord)

makePrisms ''Annotation

instance Show Annotation where
    show PrivateBuiltin = "private_builtin"
    show Private        = "private"


isBuiltin :: Annotation -> Bool
isBuiltin PrivateBuiltin = True
isBuiltin _              = False

isPrivate :: Annotation -> Bool
isPrivate _ = True
