{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
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
import Text.Printf (printf)


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

                 | LocalePathError Scope String VarPath
                 | LocaleRelativePathError Scope RawVarPath

                 | LocaleTypeError { lceExpectedType :: Type
                                   , lceGotType :: Type
                                   , lceScope :: Scope
                                   }

                 | LocaleSymbolNotFoundError Scope VarPath
                 | LocaleSignatureNotFoundError Scope VarPath [Type]
                 | LocaleSignatureConflictError [TranslationSignature]

                 | LocaleCycleError [TranslationSignature]
                 | LocaleInterfaceError String [TranslationSignature]

                 | LocaleError String
                 | LocaleUnknownError

instance Error LocaleError where
  noMsg = LocaleUnknownError
  strMsg = LocaleError

instance Show LocaleError where
  show (LocaleParseError parseError) =
      show parseError

  show (LocalePathError scope message path) =
      printf "In %s: Path error on %s: %s" (show scope) (show path) message

  show (LocaleRelativePathError scope rawPath) =
      printf "In %s: Invalid relative path %s" (show scope) (show rawPath)

  show LocaleTypeError {..} =
      printf "In %s: Type error: Expected type '%s' but found '%s'"
          (show lceScope) (show lceExpectedType) (show lceGotType)

  show (LocaleSymbolNotFoundError scope path) =
      printf "In %s: Symbol not found: %s" (show scope) (show path)

  show (LocaleSignatureNotFoundError scope path paramTypes) =
      printf "In %s: Signature not found: %s(%s)"
          (show scope) (show path) (intercalate ", " $ map show paramTypes)

  show (LocaleSignatureConflictError signatures) =
      intercalate "\n" $ "Found conflicting signatures:" : map show signatures

  show (LocaleCycleError signatures) =
      intercalate "\n" $ "Dependency cycle found:" : map show signatures

  show (LocaleInterfaceError localeName missing) =
      intercalate "\n" $ ("Missing signatures in " ++ show localeName) : map show missing

  show (LocaleError message) = message

  show LocaleUnknownError = "(unknown)"


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
  show (RawParamName name) = '@' : name

instance Show VarPath where
  show (AbsolutePath path) = intercalate "." path
  show (ParamName name) = '@' : name

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
    deriving (Eq)

instance Show Scope where
  show (TranslationScope path params) =
      printf "%s(%s)" (show path) (intercalate ", " $ map show params)

  show (SubGroupScope path) =
      printf "%s{}" (show path)


newtype Env = Env { envSignatures :: Set.Set TranslationSignature }
    deriving (Eq, Ord, Show)
