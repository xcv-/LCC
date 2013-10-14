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
  Lex.whiteSpace *>
    (Locale <$> (Lex.reserved "locale" *> Lex.identifier)
            <*> (Lex.colon *> many1 translationDataParser))
      <* eof

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
              <*> option []
                    (Lex.parens (Lex.commaSep1 paramParser) <* Lex.symbol "->")
              <*> exprParser

paramParser :: Parser Param
paramParser = Param <$> typeParser <*> Lex.identifier

exprParser :: Parser RawExpr
exprParser = try (IntLiteral    <$> Lex.intLiteral)
         <|> try (DoubleLiteral <$> Lex.floatLiteral)
         <|> try (BoolLiteral   <$> Lex.boolLiteral)
         <|> try (CharLiteral   <$> Lex.charLiteral)
         <|> try (parseString   =<< Lex.stringLiteral)
         <|> try (ArrayLiteral  <$> Lex.brackets (Lex.commaSep exprParser))
         <|> try (Conditional   <$> (Lex.reserved "if"   *> exprParser)
                                <*> (Lex.reserved "then" *> exprParser)
                                <*> (Lex.reserved "else" *> exprParser))
         <|> try (Funcall       <$> pathIdentifier
                                <*> option []
                                      (Lex.parens $ Lex.commaSep exprParser))
         <?> "expression"

parseString :: String -> Parser RawExpr
parseString str =
    case parse stringParser str (T.pack str) of
        Right result -> return $ StringConcat result
        Left err -> unexpected "String parse error"

stringParser :: Parser [RawExpr]
stringParser = many partialStringParser <* eof

partialStringParser :: Parser RawExpr
partialStringParser = try expr
                  <|> try lit
                  <?> "expression or literal"
  where
    expr = between (Lex.symbol "${") (char '}') exprParser
       <?> "brace-enclosed expression"

    lit  = StringLiteral <$> many1 (try literalChar)
       <?> "string literal"

    literalChar :: Parser Char
    literalChar = try (char '$' *> char '$')
              <|> try (noneOf "$")
              <?> "literal character"


pathIdentifier :: Parser RawVarPath
pathIdentifier = RawAbsolutePath <$> ((:) <$> Lex.identifier
                                          <*> option [] (Lex.dot *> relative))
             <|> RawParamName    <$> (char '@' *> Lex.identifier)
             <|> RawRelativePath <$> relative
             <?> "variable path"
  where
    relative = sepBy1 (Lex.symbol "^" <|> Lex.identifier) Lex.dot

typeParser :: Parser Type
typeParser = rawTypeParser <**> (arrayTypeParser <|> pure id)
         <?> "type"
  where
    rawTypeParser :: Parser Type
    rawTypeParser = (TInt    <$ Lex.reserved "int")
                <|> (TDouble <$ Lex.reserved "double")
                <|> (TBool   <$ Lex.reserved "bool")
                <|> (TChar   <$ Lex.reserved "char")
                <|> (TString <$ Lex.reserved "string")
                <?> "type name"

    arrayTypeParser :: Parser (Type -> Type)
    arrayTypeParser = TArray <$ Lex.symbol "[" <* Lex.symbol "]"
                  <?> "array type"
