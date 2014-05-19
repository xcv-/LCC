{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Parser
  ( parseLocale
  , localeParser

  , parseString
  , stringParser
  , exprParser
  ) where

import Control.Applicative hiding ((<|>), many)
import Control.Lens ((|>), (<|))
import Control.Monad
import Control.Monad.Error

import Data.Char
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import Text.Parsec
import Text.Parsec.Text

import Language.LCC.Types
import qualified Language.LCC.Error as Err
import qualified Language.LCC.Lexer as Lex


type RawTranslation = Translation RelativeVarPath UnknownType
type RawAST = AST RelativeVarPath UnknownType
type RawExpr = Expr RelativeVarPath


parseLocale :: Err.ErrorM m => String -> Text.Text -> m RawLocale
parseLocale filename fileContents =
    case parse localeParser filename fileContents of
      Right x -> return x
      Left e  -> throwError $ Err.Parse e

localeParser :: Parser RawLocale
localeParser =
    Lex.whiteSpace *>
      (Locale <$> (Lex.reserved "locale" *> Lex.identifier)
              <*> (Lex.colon *> subtreeParser mempty))
        <* eof


nodeParser :: AbsolutePath -> Parser (PathNode, RawAST)
nodeParser path = do
    name <- Lex.identifier
    (,) name <$> contentParser (path |> name)
  where
    contentParser :: AbsolutePath -> Parser RawAST
    contentParser path = Lex.braces (subtreeParser path)
                     <|> Leaf <$> translationParser path
                     <?> "translation definition or subgroup"


subtreeParser :: AbsolutePath -> Parser RawAST
subtreeParser path = Subtree . Map.fromList <$> many (nodeParser path)
                 <?> "list of translation definitions or subgroups"

translationParser :: AbsolutePath -> Parser RawTranslation
translationParser path = do
    params <- Lex.parens (Lex.commaSep1 paramParser) <* Lex.symbol "->"

    let sig = Signature path params UnknownType

    Translation sig <$> exprParser <*> getPosition


paramParser :: Parser Param
paramParser = Param <$> typeParser <*> Lex.identifier

exprParser :: Parser RawExpr
exprParser = try (IntL        <$> Lex.intLiteral)
         <|> try (DoubleL     <$> Lex.floatLiteral)
         <|> try (BoolL       <$> Lex.boolLiteral)
         <|> try (CharL       <$> Lex.charLiteral)
         <|> try (parseString =<< Lex.stringLiteral)
         <|> try (Array       <$> Lex.brackets (Lex.commaSep exprParser))

         <|> try (Funcall <$> functionPath <*> option [] argsParser)

         <|> try (Cond <$> (Lex.reserved "if"   *> exprParser)
                       <*> (Lex.reserved "then" *> exprParser)
                       <*> (Lex.reserved "else" *> exprParser))
         <?> "expression"
  where
    argsParser = Lex.parens $ Lex.commaSep exprParser


parseString :: String -> Parser RawExpr
parseString str =
    case parse stringParser str (Text.pack str) of
        Right result -> return $ SConcat result
        Left err     -> unexpected "String parse error"

stringParser :: Parser [RawExpr]
stringParser = many stringChunksParser <* eof

stringChunksParser :: Parser RawExpr
stringChunksParser = try expr
                  <|> try lit
                  <?> "expression or literal"
  where
    expr = between (Lex.symbol "${") (char '}') exprParser
       <?> "brace-enclosed expression"

    lit  = StringL <$> many1 (try literalChar)
       <?> "string literal"

    literalChar :: Parser Char
    literalChar = try (char '$' *> char '$')
              <|> try (noneOf "$")
              <?> "literal character"


functionPath :: Parser RelativeVarPath
functionPath = RVParamName    <$> (char '@' *> Lex.identifier)
           <|> RVRelativePath <$> ((<|) <$> Lex.symbol "^" <*> toSeq relTail)
           <|> RVAbsolutePath <$> ((<|) <$> Lex.identifier <*> toSeq absTail)
           <?> "variable path"
  where
    toSeq = (Seq.fromList <$>)
    relTail = many $ Lex.dot *> (Lex.symbol "^" <|> Lex.identifier)
    absTail = many $ Lex.dot *> Lex.identifier

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
