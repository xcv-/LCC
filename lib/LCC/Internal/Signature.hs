module Signature where

import LCC.Internal.Path


type RelativeGlobalSignature = Signature RelativePath
type RelativeVarSignature    = Signature RelativeVarPath

type GlobalSignature = Signature AbsolutePath
type VarSignature    = Signature AbsoluteVarPath


data Signature path = Signature
                      { _sigPath :: path
                      , _sigParams :: [Param]
                      , _sigReturn :: Type
                      }
    deriving (Eq, Ord)


data Param = Param { _paramType :: Type
                   , _paramName :: String
                   }
    deriving (Eq, Ord)

data Type = TInt
          | TDouble
          | TBool
          | TChar
          | TString
          | TArray { _arrayType :: Type }
          | TAny
    deriving (Eq, Ord)



instance Show path => Show (Signature path) where
  show Signature {} =
      show path ++ ": (" ++ paramList ++ ") -> " ++ show ret
    where
      paramList = intercalate ", " $ map show params

instance Show Param where
  show param@Param {} =
      show (param^.paramType) ++ " " ++ (param^.paramName)

instance Show Type where
  show TInt       = "int"
  show TDouble    = "double"
  show TBool      = "bool"
  show TChar      = "char"
  show TString    = "string"
  show (TArray t) = "[" ++ show t ++ "]"
  show TAny       = "<?>"



makeLenses ''Signature
makeLenses ''Type
