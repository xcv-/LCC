{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LCC.Internal.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Data.Function
import Data.List
import qualified Data.Set as Set

import Text.Parsec.Error (ParseError)


newtype LC a = LC (StateT LocaleState (ErrorT LocaleError Identity) a)
    deriving ( Functor, Applicative, Monad
             , MonadState LocaleState
             , MonadError LocaleError
             )

runLocale :: LocaleState -> LC a -> Either LocaleError (a, LocaleState)
runLocale st (LC x) = runIdentity . runErrorT $ runStateT x st


data LocaleState = LocaleState
    { lcsScope :: Scope
    , lcsEnv :: Env
    }

data LocaleError = LocaleParseError ParseError
                 | LocaleTypeError { lceExpectedType :: Type
                                   , lceGotType :: Type
                                   , lceScope :: Scope
                                   }
                 | LocaleSymbolNotFoundError Scope VarPath
                 | LocaleCycleError [TranslationSignature]
                 | LocaleSignatureNotFoundError Scope VarPath [Type]
                 | LocaleInterfaceError Locale [TranslationSignature]
                 | LocalePathError String VarPath
                 | LocaleRelativePathError Scope RawVarPath
                 | LocaleError String
                 | LocaleUnknownError
    deriving (Show)

instance Error LocaleError where
  noMsg = LocaleUnknownError
  strMsg = LocaleError


type Locale = GenericLocale VarPath
type RawLocale = GenericLocale RawVarPath

data GenericLocale path = Locale { lcName :: String
                                 , lcData :: [GenericTranslationData path]
                                 }
    deriving (Show)


type TranslationData = GenericTranslationData VarPath
type RawTranslationData = GenericTranslationData RawVarPath

data GenericTranslationData path =
        Translation { tdKey    :: String
                    , tdParams :: [Param]
                    , tdImpl   :: GenericExpr path
                    }
      | NestedData { tdSubGroupName :: String
                   , tdNestedData :: [GenericTranslationData path]
                   }
    deriving (Show)


-- Signatures and expressions

type TranslationSignature = GenericSignature AbsVarPath
type SymbolSignature = GenericSignature VarPath
type RawSignature = GenericSignature RawVarPath

data GenericSignature path = Signature
                               { sigPath :: path
                               , sigParams :: [Param]
                               , sigReturn :: Type
                               }
    deriving (Eq)


data Param = Param { paramType :: Type
                   , paramName :: String
                   }
    deriving (Eq)

data Type = TInt
          | TDouble
          | TBool
          | TChar
          | TString
          | TArray Type
          | TAny
    deriving (Eq, Ord)


instance Ord path => Ord (GenericSignature path) where
  compare sig1 sig2 =
      case compare (sigPath sig1) (sigPath sig2) of
          EQ -> compare (sigParams sig1) (sigParams sig2)
          x -> x

instance Ord Param where
  compare = compare `on` paramType


instance Show path => Show (GenericSignature path) where
  show Signature { sigPath=path, sigParams=params, sigReturn=ret } =
      show path ++ ": (" ++ paramList ++ ") -> " ++ show ret
    where
      paramList = intercalate ", " $ map show params

instance Show Param where
  show Param { paramType=pType, paramName=name } = show pType ++ " " ++ name

instance Show Type where
  show TInt       = "int"
  show TDouble    = "double"
  show TBool      = "bool"
  show TChar      = "char"
  show TString    = "string"
  show (TArray t) = show t ++ "[]"
  show TAny       = "<?>"


-- Paths

data RawVarPath = RawAbsolutePath [String]
                | RawRelativePath [String]
                | RawParamName    String
    deriving (Eq, Ord)

data VarPath = AbsolutePath  [String]
             | ParamName String
    deriving (Eq, Ord)

data AbsVarPath = AbsVarPath [String]
    deriving (Eq, Ord)


instance Show RawVarPath where
  show (RawAbsolutePath path) = intercalate "." path
  show (RawRelativePath path) = intercalate "." path
  show (RawParamName name) = "@" ++ name

instance Show VarPath where
  show (AbsolutePath path) = intercalate "." path
  show (ParamName name) = "@" ++ name

instance Show AbsVarPath where
  show (AbsVarPath path) = intercalate "." path


-- Expressions

type Expr = GenericExpr VarPath
type RawExpr = GenericExpr RawVarPath

data GenericExpr path = IntLiteral    Integer
                      | DoubleLiteral Double
                      | BoolLiteral   Bool
                      | CharLiteral   Char
                      | StringLiteral String
                      | StringConcat  [GenericExpr path]
                      | ArrayLiteral  [GenericExpr path]
                      | Conditional   (GenericExpr path)
                                      (GenericExpr path)
                                      (GenericExpr path)
                      | Funcall       path [GenericExpr path]
    deriving (Ord, Eq, Show)


-- Environment

data Scope = TranslationScope AbsVarPath [Param]
           | SubGroupScope AbsVarPath
    deriving (Eq, Show)

newtype Env = Env { envSignatures :: Set.Set TranslationSignature }
    deriving (Eq, Ord, Show)
