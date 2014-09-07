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


parseLocale :: Err.ErrorM m => String -> Text.Text -> m (Bool, RawLocale, [Import])
parseLocale filename fileContents =
    case parse localeParser filename fileContents of
      Right x -> return x
      Left e  -> Err.parsingError e

localeParser :: Parser (Bool, RawLocale, [Import])
localeParser = do
    Lex.whiteSpace

    abstract <- option False (True <$ Lex.reserved "private")

    Lex.reserved "locale"

    name <- Lex.identifier

    inputs <- option [] $
                  sepBy (do anns  <- many annotationParser
                            param <- paramParser
                            return (LocaleInput param anns))
                        Lex.comma
    Lex.semicolon

    imports <- many importParser

    ast <- subtreeParser mempty []
    eof

    return (abstract, Locale name inputs ast, imports)


importParser :: Parser Import
importParser = do
    path <- Lex.reserved "import" *> Lex.identifier
    name <- Lex.reserved "as"     *> Lex.identifier

    return (Import path name)


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


translationParser :: AbsolutePath -> [Annotation] -> Parser RawTranslation
translationParser path anns = do
    pos <- getPosition

    params <- option [] $
                Lex.parens (Lex.commaSep1 paramParser) <* Lex.symbol "->"

    expr <- exprParser

    let sig = Signature path params UnknownType

    return $ Translation sig expr anns pos


annotationsParser :: [Annotation] -> Parser [Annotation]
annotationsParser anns = do
    anns' <- option [] $ Lex.brackets (Lex.commaSep annotationParser)

    return $ nub (anns ++ anns')

annotationParser :: Parser Annotation
annotationParser = Private <$ Lex.reserved "private"
               <?> "annotation"


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
        Left  e      -> unexpected $ "String parse error: " ++ show e

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
functionPath = RVParamName    <$> paramNameParser
           <|> RVRelativePath <$> relativePathParser
           <|> RVAbsolutePath <$> absolutePathParser
           <?> "variable path"

absolutePathParser :: Parser (Seq.Seq PathNode)
absolutePathParser = Seq.fromList <$> sepBy1 Lex.identifier Lex.dot

paramNameParser :: Parser PathNode
paramNameParser = char '@' *> Lex.identifier

relativePathParser :: Parser (Seq.Seq PathNode)
relativePathParser = do
    let up = Lex.symbol "^"

    h <- up <* Lex.dot
    t <- sepBy1 (up <|> Lex.identifier) Lex.dot

    return $ Seq.fromList (h <| t)


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
