{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Parser
  ( parseLocale
  , localeParser

  , parseString
  , stringParser
  , exprParser
  ) where

import Control.Applicative hiding ((<|>), many)
import Control.Lens (traverse, _2)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Except

import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import Text.Parsec
import Text.Parsec.Text

import Language.LCC.AST
import qualified Language.LCC.Error as Err
import qualified Language.LCC.Lexer as Lex


parseLocale :: Err.ErrorM m => String -> Text.Text -> m RawLocale
parseLocale filename fileContents =
    case parse localeParser filename fileContents of
      Right x -> return x
      Left e  -> throwError $ Err.Parse e

localeParser :: Parser RawLocale
localeParser = do
    Lex.whiteSpace
    Lex.reserved "locale"

    name <- Lex.identifier
    Lex.colon

    ast <- subtreeParser mempty []

    return (Locale name ast)


nodeParser :: AbsolutePath -> [Annotation] -> Parser (PathNode, RawAST)
nodeParser path anns = do
    name  <- Lex.identifier
    anns' <- annotationsParser anns

    Lex.colon
    content <- contentParser (path |> name) anns'

    return (name, content)
  where
    contentParser :: AbsolutePath -> [Annotation] -> Parser RawAST
    contentParser path' anns' = Lex.braces (subtreeParser path' anns')
                            <|> Leaf . pure <$> translationParser path' anns'
                            <?> "translation definition or subgroup"


subtreeParser :: AbsolutePath -> [Annotation] -> Parser RawAST
subtreeParser path anns = do
    nodes <- many (nodeParser path anns)
         <?> "list of translation definitions or subgroups"

    m <- buildMap nodes

    return (Subtree m)
  where
    buildMap :: [(PathNode, RawAST)] -> Parser (Map.Map PathNode RawAST)
    buildMap = liftM Map.fromList
             . (traverse._2 %%~ joinLeafs)
             . groupByFst
             . sortBy (compare`on`fst)

    joinLeafs :: [RawAST] -> Parser RawAST
    joinLeafs nss =
        case nss of
          Leaf _:_    -> Leaf   <$> mapM toLeaf nss
          [Subtree m] -> return  $  Subtree m
          _           -> fail    $  "Found repeated subtrees: " ++ show nss

    toLeaf :: RawAST -> Parser RawTranslation
    toLeaf (Leaf [x]) = return x
    toLeaf (Leaf xs)  = fail $ "Expected single leaf, found " ++ show xs
    toLeaf subtree    = fail $ "Expected leaf found subtree: " ++ show subtree


annotationsParser :: [Annotation] -> Parser [Annotation]
annotationsParser anns = do
    anns' <- option [] $ Lex.brackets (Lex.commaSep annotationParser)

    return $ nub (anns ++ anns')
  where
    annotationParser = Private <$ Lex.reserved "private"
                   <?> "annotation"

translationParser :: AbsolutePath -> [Annotation] -> Parser RawTranslation
translationParser path anns = do
    pos <- getPosition

    params <- option [] $
                Lex.parens (Lex.commaSep1 paramParser) <* Lex.symbol "->"

    expr <- exprParser

    let sig = Signature path params UnknownType

    return $ Translation sig expr anns pos


paramParser :: Parser Param
paramParser = Param <$> typeParser <*> Lex.identifier

exprParser :: Parser RawExpr
exprParser = try (IntL        <$> Lex.intLiteral)
         <|> try (DoubleL     <$> Lex.floatLiteral)
         <|> try (BoolL       <$> Lex.boolLiteral)
         <|> try (CharL       <$> Lex.charLiteral)
         <|> try (parseString =<< Lex.stringLiteral)
         <|> try (Array       <$> Lex.brackets (Lex.commaSep exprParser))

         <|> try (Funcall . Fn <$> functionPath <*> option [] argsParser)

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
        Left  _      -> unexpected "String parse error"

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
