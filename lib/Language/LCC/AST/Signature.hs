{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.AST.Signature where

import Control.Lens

import Data.List
import Data.Maybe

import Language.LCC.AST.Path



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

type RelSignature ret  = Signature RelativePath ret
type AbsSignature ret  = Signature AbsolutePath ret

type RawSignature      = Signature RelativePath UnknownType
type AnalyzedSignature = Signature AbsolutePath Type


instance (Show path, Show ret) => Show (Signature path ret) where
  show Signature {..} =
      show _sigPath ++ ": (" ++ paramList ++ ") -> " ++ show _sigReturn
    where
      paramList = intercalate ", " $ map show _sigParams


paramNameEq :: String -> Param -> Bool
paramNameEq name param = param^.paramName == name


paramSig :: FromParamName path => Param -> Signature path Type
paramSig Param {..} = Signature { _sigPath = _ParamName # _paramName
                                , _sigParams = []
                                , _sigReturn = _paramType
                                }

sigParamTypes :: Traversal' (Signature path ret) Type
sigParamTypes = sigParams.traverse.paramType

sigParamCount :: Getter (Signature path ret) Int
sigParamCount = sigParams.to length


matchSig :: Eq path => Signature path ret -> Signature path ret -> Bool
matchSig s1 s2 = s1^.sigPath     == s2^.sigPath
              && s1^.sigParamCount == s2^.sigParamCount
              && s1^..sigParamTypes == s2^..sigParamTypes



partMatchParams1 :: [Maybe Type] -> [Type] -> Bool
partMatchParams1 ps1 ps2 = partMatchParams2 ps1 (map Just ps2)

partMatchParams2 :: [Maybe Type] -> [Maybe Type] -> Bool
partMatchParams2 ps1 ps2 = length ps1 == length ps2
                       && and (zipWith match ps1 ps2)
  where
    match :: Maybe Type -> Maybe Type -> Bool
    match p1 p2 = isNothing p1 || isNothing p2 || p1 == p2
