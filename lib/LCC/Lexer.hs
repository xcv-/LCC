module LCC.Lexer
  ( whiteSpace
  , lexeme
  , symbol
  , reserved
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
import Control.Applicative hiding ((<|>), many)
import Control.Monad.Identity (Identity)

import Text.Parsec.Text ()

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.List
import qualified Data.Text as T


lcKeywords, javaKeywords, cppKeywords :: [String]

lcKeywords = ["locale", "int",    "double", "bool",
              "char",   "string", "true",   "false",
              "if",     "then",   "else"]

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
  { Tok.commentStart   = ""
  , Tok.commentEnd     = ""
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

lcc = Tok.makeTokenParser lccDef

whiteSpace     = Tok.whiteSpace    lcc
lexeme         = Tok.lexeme        lcc
symbol         = Tok.symbol        lcc
reserved       = Tok.reserved      lcc
parens         = Tok.parens        lcc
braces         = Tok.braces        lcc
brackets       = Tok.brackets      lcc
dot            = Tok.dot           lcc
colon          = Tok.colon         lcc
comma          = Tok.comma         lcc
commaSep       = Tok.commaSep      lcc
commaSep1      = Tok.commaSep1     lcc

identifier     = Tok.identifier    lcc
intLiteral     = Tok.integer       lcc
floatLiteral   = Tok.float         lcc
charLiteral    = Tok.charLiteral   lcc
stringLiteral  = Tok.stringLiteral lcc

boolLiteral    = (True  <$ symbol "true")
           P.<|> (False <$ symbol "false")
           P.<?> "boolean literal"
