{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Monad

import Data.Functor
import Data.List (foldl1')

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Text.PrettyPrint.Leijen.Text (putDoc, pretty)

import System.Directory
import System.Environment
import System.Exit
import System.FilePath

import Language.LCC.AST
import Language.LCC.Analyzer
import Language.LCC.Parser
import Language.LCC.Pretty
import Language.LCC.Target
import Language.LCC.Targets.Java
import Language.LCC.Simplifier.Inline
import Language.LCC.Simplifier.RemoveUnused
import qualified Language.LCC.Error as Err



main :: IO ()
main = do
    args <- getArgs

    rc <- case args of
            ["help"] ->
                printHelp

            ["help", topic] ->
                printTopicHelp topic

            ["parse", indir] ->
                printParsed indir

            ["compile", "java", indir, outdir, package] ->
                compileToJava indir outdir package

            _ -> do
                putStrLn "Invalid arguments"
                printHelp
                return (ExitFailure 1)

    exitWith rc


printHelp :: IO ExitCode
printHelp = do
    progName <- getProgName

    mapM_ putStrLn
      [ "Syntax: " ++ progName ++ " compile <target-language> <opts...>"
      , "Available target languages:"
      , "\tjava"
      , ""
      , "Help: " ++ progName ++ " help topic"
      , "Available help topics:"
      , "\tlc-lang"
      , "\tjava"
      ]

    return ExitSuccess


printTopicHelp :: String -> IO ExitCode
printTopicHelp "lc-lang" = do
    mapM_ putStrLn
      [ "LC is a simple declarative language for static application localization."
      , "It compiles directly to units of a given output language so it can be"
      , "compiled statically into the application, allowing for compile-time"
      , "checking of values and parameter types."
      , ""
      , "All elements are a value and key pair in a locale '.lc' file. The name of"
      , "the locale is specified in the beginning of the file with a 'locale NAME:'"
      , "statement, where NAME is the desired name of a locale (eg. en_US)."
      , ""
      , "The LC language is very simple and consists of only two main syntactical"
      , "elements: translations and subgroups. Subgroups are brace-enclosed"
      , "groups of translations or more subgroups and translations are function"
      , "definitions. These pairs are represented with 'key: value' syntax."
      , "Example file:"
      , ""
      , "locale en_US:"
      , ""
      , "# comments are allowed"
      , "# myApplication is a subgroup"
      , "myApplication: {"
      , "\tmyTranslation1: \"Translated text goes here\""
      , "\tmyTranslation2: \"Translations are separated by newlines\""
      , ""
      , "\t# ${} blocks are interpolated"
      , "\t# use @var to reference an argument"
      , "\t# path.to.var to access other elements or builtins"
      , "\tmyParameterizedTranslation: (int x) -> \"Value of x is ${str(@x)}.\""
      , ""
      , "\tsubGroupsCanBeNested: {"
      , "\t\tdeeperTranslation: \"More text\""
      , "\t\treferencingOthers: (int x) -> \"Here I call a translation: ${^.^.myParameterizedTranslation(@x)}\""
      , "\t}"
      , ""
      , "\tnotOnlyStringsAreAllowed: 3"
      , "\tlistsAvailableAsWell: [ 42, 1337 ]"
      , "}"
      , ""
      , "At compilation time, all locale files are scanned, checked for type-correctness,"
      , "inconsistencies, unknown function calls and missing translations."
      ]
    return ExitSuccess

printTopicHelp "java" = do
    progName <- getProgName

    mapM_ putStrLn
      [ "Syntax: " ++ progName
                   ++ " compile"
                   ++ " java"
                   ++ " <input-directory>"
                   ++ " <output-directory>"
                   ++ " <package-name>"
      , "input-directory: Directory where locale files are contained"
      , "output-directory: Output source tree root"
      , "package-name: Java package name for the class"
      , ""
      , "Note: The name of the result interface is input-directory"
      ]
    return ExitSuccess


printTopicHelp topic = do
    progName <- getProgName

    mapM_ putStrLn
      [ "Unknown help topic: " ++ topic
      , "Note: Type '" ++ progName ++ " help' for a list of topics"
      ]
    return (ExitFailure 2)



printParsed :: FilePath -> IO ExitCode
printParsed indir = do
    files <- loadFiles indir

    let parsed = mapM (uncurry parseLocale) files

    either displayError printPrettified parsed
  where
    printPrettified ls = ExitSuccess <$ do
        forM_ ls $ \l -> do
          putStrLn $ "##### " ++ _localeName l ++ " #####"
          putDoc (pretty l)
          putStrLn "" >> putStrLn ""



compileToJava :: FilePath -> FilePath -> String -> IO ExitCode
compileToJava indir outdir package =
    compileGeneric indir
      JavaTarget
        { javaTabWidth      = 4
        , javaExpandTab     = False
        , javaDirname       = outdir
        , javaPackage       = package
        , javaInterfaceName = last $ splitDirectories indir
        }


compileGeneric :: Target t => FilePath -> t -> IO ExitCode
compileGeneric indir target = do
    files <- loadFiles indir

    let processed = processLocales target files

    either displayError writeOutput processed


processLocales :: (Applicative m, Err.ErrorM m, Target t)
               => t
               -> [(FilePath, T.Text)]
               -> m [(FilePath, TL.Text)]
processLocales target files = do
    parsed     <- mapM (uncurry parseLocale) files
    analyzed   <- mapM (analyze target) parsed

    let simplify = foldl1' (>=>)
                 [ return
                 , localeAST (inline 20)
                 , localeAST removeUnused
                 ]

    simplified <- mapM simplify analyzed

    output target simplified



loadFiles :: FilePath -> IO [(FilePath, T.Text)]
loadFiles directory = do
    files <- getDirectoryFiles directory

    forM files $ \file -> do
        putStrLn ("Found file: " ++ file)
        content <- T.readFile file
        return (file, content)
  where
    getDirectoryFiles :: FilePath -> IO [FilePath]
    getDirectoryFiles dir = do
        contents <- getDirectoryContents dir

        filterM doesFileExist $ map (dir </>) contents


writeOutput :: [(FilePath, TL.Text)] -> IO ExitCode
writeOutput outputData = do
    forM_ outputData $ \(file, content) -> do
        createDirectoryIfMissing True (takeDirectory file)
        putStrLn $ "Writing " ++ file
        TL.putStrLn content

    putStrLn "Done."
    return ExitSuccess


displayError :: Err.Error -> IO ExitCode
displayError e = do
    putStrLn $ "\n* Error: " ++ show e
    putStrLn "\nErrors occurred, aborting"
    return (ExitFailure 3)
