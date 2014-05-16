module LCC.Internal.Signature where

import LCC.Internal.Path


type Parsed p t = t p ()
type Analyzed p t = t p ()

type RelGlobal = RelativePath
type RelVar    = RelativeVarPath

type Global = AbsolutePath
type Var    = AbsoluteVarPath


data Signature path ret = Signature
                          { _sigPath :: path
                          , _sigParams :: [Param]
                          , _sigReturn :: ret
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



instance (Show path, Show ret) => Show (Signature path ret) where
  show sig =
      show (sig^.sigPath) ++ ": (" ++ paramList ++ ") -> " ++ show _sigReturn
    where
      paramList = intercalate ", " $ map show _sigParams

instance Show Param where
  show param =
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
