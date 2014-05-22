{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.Types.Signature where

import Control.Lens

import Data.List

import Language.LCC.Types.Path



data UnknownType = UnknownType
    deriving (Eq, Ord)


instance Show UnknownType where
  show UnknownType = "<?>"


data Type = TInt
          | TDouble
          | TBool
          | TChar
          | TString
          | TArray { _arrayType :: Type }
    deriving (Eq, Ord)

makeLenses ''Type

instance Show Type where
  show TInt       = "int"
  show TDouble    = "double"
  show TBool      = "bool"
  show TChar      = "char"
  show TString    = "string"
  show (TArray t) = "[" ++ show t ++ "]"



data Param = Param { _paramType :: Type
                   , _paramName :: String
                   }
    deriving (Eq, Ord)

makeLenses ''Param


instance Show Param where
  show Param {..} =
      show _paramType ++ " " ++ _paramName



data Signature path ret = Signature
                          { _sigPath :: path
                          , _sigParams :: [Param]
                          , _sigReturn :: ret
                          }
    deriving (Eq, Ord)

makeLenses ''Signature


instance (Show path, Show ret) => Show (Signature path ret) where
  show Signature {..} =
      show _sigPath ++ ": (" ++ paramList ++ ") -> " ++ show _sigReturn
    where
      paramList = intercalate ", " $ map show _sigParams

