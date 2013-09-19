module LCC.Parser
  ( parseLocale
  , localeParser

  , parseString
  , stringParser
  , exprParser
  ) where

import Data.Char
import Data.Monoid
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Control.Monad.Identity
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Text

import LCC.Internal.Types
import qualified LCC.Lexer as Lex


parseLocale :: String
            -> T.Text
            -> Either LocaleError RawLocale
parseLocale filename fileContents =
  case parse localeParser filename fileContents of
      Right x -> Right x
      Left e  -> Left $ LocaleParseError e

localeParser :: Parser RawLocale
localeParser =
  Locale <$> (Lex.whiteSpace *> Lex.symbol "locale" *> Lex.identifier)
         <*> (Lex.colon *> many1 translationDataParser)

translationDataParser :: Parser RawTranslationData
translationDataParser = try nestedDataParser
                    <|> try translationParser
                    <?> "translation definition or subgroup"


nestedDataParser :: Parser RawTranslationData
nestedDataParser =
  NestedData <$> Lex.identifier
             <*> (Lex.colon *> Lex.braces (many1 translationDataParser))


translationParser :: Parser RawTranslationData
translationParser =
  Translation <$> (Lex.identifier <* Lex.colon)
              <*> (option [] $
                    Lex.parens (Lex.commaSep1 paramParser) <* Lex.symbol "->")
              <*> exprParser

paramParser :: Parser Param
paramParser = Param <$> typeParser <*> Lex.identifier

exprParser :: Parser RawExpr
exprParser = try (CharLiteral   <$> Lex.charLiteral)
         <|> try (IntLiteral    <$> Lex.intLiteral)
         <|> try (DoubleLiteral <$> Lex.floatLiteral)
         <|> try (parseString   =<< Lex.stringLiteral)
         <|> try (ArrayLiteral  <$> Lex.brackets (Lex.commaSep exprParser))
         <|> try (Funcall       <$> pathIdentifier
                                <*> (option [] $
                                      Lex.parens (Lex.commaSep exprParser)))
         <?> "expression"

parseString :: String -> Parser RawExpr
parseString str =
    case parse stringParser str (T.pack str) of
        Right result -> return $ StringConcat result
        Left err -> unexpected "String parse error"

stringParser :: Parser [RawExpr]
stringParser = many1 partialStringParser <* eof

partialStringParser :: Parser RawExpr
partialStringParser = try expr
                  <|> try lit
                  <?> "expression or literal"
  where
    expr = between (Lex.symbol "${") (char '}') exprParser
       <?> "brace-enclosed expression"

    lit  = StringLiteral <$> many1 (try literalChar)
       <?> "translation literal"

    literalChar :: Parser Char
    literalChar = try (char '$' *> char '$')
              <|> try (noneOf "$")
              <?> "literal character"


pathIdentifier :: Parser RawVarPath
pathIdentifier = RawAbsolutePath <$> ((:) <$> Lex.identifier
                                          <*> (option [] $ Lex.dot *> relative))
             <|> RawParamName    <$> (char '@' *> Lex.identifier)
             <|> RawRelativePath <$> relative
             <?> "variable path"
  where
    relative = sepBy1 (Lex.symbol "^" <|> Lex.identifier) Lex.dot

typeParser :: Parser Type
typeParser = (TInt    <$ Lex.symbol "int")
         <|> (TDouble <$ Lex.symbol "double")
         <|> (TChar   <$ Lex.symbol "char")
         <|> (TString <$ Lex.symbol "string")
         <|> (TArray  <$> (typeParser <* Lex.symbol "[" <* Lex.symbol "]"))
         <?> "type name"

