module LCC.Internal.Expr

import Control.Lens.Prism
import Control.Lens.TH

import LCC.Internal.Path

type ReadyExpr = Expr AbsolutePath

data Expr path
    = IntLiteral    { _exprInt     :: Integer     }
    | DoubleLiteral { _exprDouble  :: Double      }
    | BoolLiteral   { _exprBool    :: Bool        }
    | CharLiteral   { _exprChar    :: Char        }
    | StringLiteral { _exprString  :: String      }
    | StringConcat  { _exprStrings :: [Expr path] }
    | ArrayLiteral  { _exprArray   :: [Expr path] }

    | Funcall       { _exprFunction :: path
                    , _exprArgs     :: [Expr path]
                    }

    | Conditional   { _exprCondition :: Expr path
                    , _exprConsequent :: Expr path
                    , _exprAlternative :: Expr path
                    }

    deriving (Ord, Eq, Show)



makeLenses ''Expr


-- Prisms

_int :: SimplePrism (Expr path) Integer
_int = prism IntLiteral $ \case of
    IntLiteral x -> Right x
    x            -> Left x

_double :: SimplePrism (Expr path) Double
_double = prism DoubleLiteral $ \case of
    DoubleLiteral x -> Right x
    x               -> Left x

_bool :: SimplePrism (Expr path) Bool
_bool = prism BoolLiteral $ \case of
    BoolLiteral x -> Right x
    x             -> Left x

_char :: SimplePrism (Expr path) Char
_char = prism CharLiteral $ \case of
    CharLiteral x -> Right x
    x             -> Left x

_string :: SimplePrism (Expr path) String
_string = prism StringLiteral $ \case of
    StringLiteral x -> Right x
    x               -> Left x

_stringConcat :: SimplePrism (Expr path) [Expr path]
_stringConcat = prism StringConcat $ \case of
    StringConcat x -> Right x
    x              -> Left x

_array :: SimplePrism (Expr path) [Expr path]
_array = prism ArrayLiteral $ \case of
    ArrayLiteral x -> Right x
    x              -> Left x
