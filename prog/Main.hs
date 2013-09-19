module Main where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import System.Directory
import System.Environment
import System.FilePath

import qualified Data.Text as T
import qualified Data.Text.IO as T

import LCC.Parser
import LCC.State
import LCC.Analyzer
import LCC.Types
import LCC.Target
import qualified LCC.Output.Java as J


compileLocales :: Target t => t -> [RawLocale] -> [LC Locale]
compileLocales target = map process
  where
    process :: RawLocale -> LC Locale
    process locale = setEnv target >> compile locale


parseLocales :: FilePath -> IO [Either LocaleError RawLocale]
parseLocales directory = do
    files <- getDirectoryFiles directory

    forM files $ \file -> do
        putStrLn $ "Found file: " ++ file
        parseLocale file <$> T.readFile file
  where
    getDirectoryFiles :: FilePath -> IO [FilePath]
    getDirectoryFiles dir =
        map (dir </>) <$> getDirectoryContents dir >>= filterM doesFileExist


loadLocales :: Target t => t -> FilePath -> IO [LC Locale]
loadLocales target directory = do
    parsedLocales <- parseLocales directory

    case compileLocales target <$> sequence parsedLocales of
        Right compiledLocales  -> return compiledLocales
        Left err -> return [throwError err]


applyOutput :: Target t => t
            -> [LC Locale]
            -> Either LocaleError [(FilePath, T.Text)]
applyOutput target locales = do
    case checkInterfaces =<< mapM (runLocale emptyState) locales of
        Right [] -> Right []
        Left e -> Left e

        Right checkedLocales@((_,st0):_) ->
            fst <$> runLocale st0 (output target $ map fst checkedLocales)



writeOutput :: FilePath -> T.Text -> IO ()
writeOutput path content = do
    putStrLn $ "Generated " ++ path
    createDirectoryIfMissing True (takeDirectory path)
    T.writeFile path content


main :: IO ()
main = do
    args <- getArgs

    case args of
        ["help"] ->
            printHelp

        ["help", topic] ->
            printTopicHelp topic

        ["parse", indir] ->
            printParsed indir

        ["compile", "java", indir, outdir, package] ->
            compileToJava indir outdir package

        _ ->
            putStrLn "Invalid arguments" >> printHelp


printHelp :: IO ()
printHelp = do
    progName <- getProgName

    putStrLn . unlines $
      [ "Syntax: " ++ progName ++ " compile <target-language> <opts...>"
      , "Available target languages:"
      , "\tjava"
      , ""
      , "Help: " ++ progName ++ " help topic"
      , "Available help topics:"
      , "\tlc-lang"
      , "\tjava"
      ]


printTopicHelp :: String -> IO ()
printTopicHelp "lc-lang" = do
    putStrLn . unlines $
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

printTopicHelp "java" = do
    progName <- getProgName

    putStrLn . unlines $
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

printTopicHelp topic = do
    progName <- getProgName

    putStrLn . unlines $
      [ "Unknown help topic: " ++ topic
      , "Note: Type '" ++ progName ++ " help' for a list of topics"
      ]


printParsed :: FilePath -> IO ()
printParsed indir = do
    mapM_ print =<< parseLocales indir

compileToJava :: FilePath -> FilePath -> String -> IO ()
compileToJava indir outdir package = do
    let target = J.JavaTarget
                   { J.javaTabWidth      = 4
                   , J.javaExpandTab     = False
                   , J.javaDirname       = outdir
                   , J.javaPackage       = package
                   , J.javaInterfaceName = dropTrailingPathSeparator indir
                   }

    loadedLocales <- loadLocales target indir

    case applyOutput target loadedLocales of
        Right results -> mapM_ (uncurry writeOutput) results
        Left e -> print e

  where
    prettyPrint (file, content) = do
        putStrLn $ "// " ++ file
        T.putStrLn content
        putStrLn ""
