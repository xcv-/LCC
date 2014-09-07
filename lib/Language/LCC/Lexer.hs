module Language.LCC.Lexer
  ( whiteSpace
  , symbol
  , reserved
  , lexeme
  , angles
  , parens
  , braces
  , brackets
  , dot
  , colon
  , comma
  , commaSep
  , commaSep1

  , identifier
  , intLiteral
  , floatLiteral
  , charLiteral
  , stringLiteral
  , boolLiteral
  ) where


import Data.Functor
import Control.Monad.Identity (Identity)

import Text.Parsec.Text (Parser)

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.List
import qualified Data.Text as T


lcKeywords, javaKeywords, cppKeywords :: [String]

lcKeywords = ["locale", "int",    "double", "bool",
              "char",   "string", "true",   "false",
              "if",     "then",   "else",   "private"]

javaKeywords = ["abstract", "continue",     "for",       "new",
                "switch",   "assert",       "default",   "goto",
                "package",  "synchronized", "boolean",   "do",
                "if",       "private",      "this",      "break",
                "double",   "implements",   "protected", "throw",
                "byte",     "else",         "import",    "public",
                "throws",   "case",         "enum",      "instanceof",
                "return",   "transient",    "catch",     "extends",
                "int",      "short",        "try",       "char",
                "final",    "interface",    "static",    "void",
                "class",    "finally",      "long",      "strictfp",
                "volatile", "const",        "float",     "native",
                "super",    "while"]

cppKeywords = ["alignas",   "alignof",       "and",          "and_eq",
               "asm",       "auto",          "bitand",       "bitor",
               "bool",      "break",         "case",         "catch",
               "char",      "char16_t",      "char32_t",     "class",
               "compl",     "const",         "constexpr",    "const_cast",
               "continue",  "decltype",      "default",      "delete",
               "do",        "double",        "dynamic_cast", "else",
               "enum",      "explicit",      "export",       "extern",
               "false",     "float",         "for",          "friend",
               "goto",      "if",            "inline",       "int",
               "long",      "mutable",       "namespace",    "new",
               "noexcept",  "not",           "not_eq",       "nullptr",
               "operator",  "or",            "or_eq",        "private",
               "protected", "public",        "register",     "reinterpret_cast",
               "return",    "short",         "signed",       "sizeof",
               "static",    "static_assert", "static_cast",  "struct",
               "switch",    "template",      "this",         "thread_local",
               "throw",     "true",          "try",          "typedef",
               "typeid",    "typename",      "union",        "unsigned",
               "using",     "virtual",       "void",         "volatile",
               "wchar_t",   "while",         "xor",          "xor_eq"]

lccDef :: Tok.GenLanguageDef T.Text () Identity
lccDef = Tok.LanguageDef
  { Tok.commentStart   = "/*"
  , Tok.commentEnd     = "*/"
  , Tok.commentLine    = "#"
  , Tok.nestedComments = False
  , Tok.identStart     = P.letter
  , Tok.identLetter    = P.alphaNum P.<|> P.char '_'
  , Tok.opStart        = P.oneOf ""
  , Tok.opLetter       = P.oneOf ""
  , Tok.reservedNames  = foldl1' union [ Tok.reservedNames Lang.haskellDef
                                       , lcKeywords
                                       , javaKeywords
                                       , cppKeywords
                                       ]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive = True
  }

lc ::  Tok.GenTokenParser T.Text () Identity
lc = Tok.makeTokenParser lccDef


whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lc

symbol :: String -> Parser String
symbol = Tok.symbol lc

reserved :: String -> Parser ()
reserved = Tok.reserved lc

lexeme, angles, parens, braces, brackets :: Parser a -> Parser a
lexeme   = Tok.lexeme   lc
angles   = Tok.angles   lc
parens   = Tok.parens   lc
braces   = Tok.braces   lc
brackets = Tok.brackets lc

dot, colon, comma :: Parser String
dot   = Tok.dot   lc
colon = Tok.colon lc
comma = Tok.comma lc

commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep  = Tok.commaSep  lc
commaSep1 = Tok.commaSep1 lc


{-
identifier, intLiteral, floatLiteral, charLiteral, stringLiteral
  :: Tok.GenTokenParser () Identity
  -}

identifier :: Parser String
identifier = Tok.identifier lc

intLiteral :: Parser Integer
intLiteral = Tok.integer lc

floatLiteral :: Parser Double
floatLiteral = Tok.float lc

charLiteral :: Parser Char
charLiteral = Tok.charLiteral lc

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lc

boolLiteral :: Parser Bool
boolLiteral = (True  <$ symbol "true")
        P.<|> (False <$ symbol "false")
        P.<?> "boolean literal"
