module LCC.Internal.Error where

import Text.Printf (


data Error
    = Parse ParseError

    | Path Scope String VarPath
    | RelativePath Scope RawVarPath

    | Type { expected :: Type
           , found :: Type
           , scope :: Scope
           }

    | SymbolNotFound Scope AbsoluteVarPath
    | SignatureNotFound Scope AbsoluteVarPath [Type]
    | SignatureConflict [GlobalSignature]

    | Cycle [GlobalSignature]
    | Interface String [GlobalSignature]

    | Message String
    | Unknown

instance Show Error where
  show (Parse parseError) =
      show parseError

  show (Path scope message path) =
      printf "In %s: Path error on %s: %s" (show scope) (show path) message

  show (RelativePath scope rawPath) =
      printf "In %s: Invalid relative path %s" (show scope) (show rawPath)

  show Type {..} =
      printf "In %s: Type error: Expected type '%s' but found '%s'"
          (show scope) (show expected) (show found)

  show (SymbolNotFound scope path) =
      printf "In %s: Symbol not found: %s" (show scope) (show path)

  show (SignatureNotFound scope path paramTypes) =
      printf "In %s: Signature not found: %s(%s)"
          (show scope) (show path) (intercalate ", " $ map show paramTypes)

  show (SignatureConflict signatures) =
      intercalate "\n" $ "Found conflicting signatures:" : map show signatures

  show (Cycle signatures) =
      intercalate "\n" $ "Dependency cycle found:" : map show signatures

  show (Interface localeName missing) =
      intercalate "\n" $ ("Missing signatures in " ++ show localeName) : map show missing

  show (Message message) = message

  show Unknown = "(unknown)"

