{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import System.Directory
import System.Environment
import System.FilePath

import Text.Printf (printf)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import LCC.Parser
import LCC.State
import LCC.Analyzer
import LCC.Types
import LCC.Target
import qualified LCC.Output.Java as J


type LocaleErrorPair = (Maybe RawLocale, LocaleError)


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


compileLocale :: Target t => t
              -> RawLocale
              -> Either LocaleErrorPair (Locale, LocaleState)
compileLocale target locale =
    case runLocale emptyState compiled of
        Right x -> Right x
        Left e  -> Left (Just locale, e)
  where
    compiled = setEnv target >> compile locale


loadLocales :: Target t => t
            -> FilePath
            -> IO (Either LocaleErrorPair [(Locale, LocaleState)])
loadLocales target directory =
    mapM applyCompile <$> parseLocales directory
  where
    applyCompile :: Either LocaleError RawLocale
                 -> Either (Maybe RawLocale, LocaleError) (Locale, LocaleState)
    applyCompile (Right locale) = compileLocale target locale
    applyCompile (Left err)     = Left (Nothing, err)


writeOutput :: FilePath -> T.Text -> IO ()
writeOutput path content = do
    putStrLn $ "Generating " ++ path
    createDirectoryIfMissing True (takeDirectory path)
    T.writeFile path content
    putStrLn "Done."


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
printTopicHelp "lc-lang" =
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
printParsed indir = mapM_ print =<< parseLocales indir


compileGeneric :: Target t => FilePath -> t -> IO ()
compileGeneric indir target =
    loadLocales target indir >>= either printErrorPair process
  where
    process :: [(Locale, LocaleState)] -> IO ()
    process = either printError writeAll . (checkInterfaces >=> runOutput)

    runOutput :: [(Locale, LocaleState)] -> Either LocaleError [(FilePath, T.Text)]
    runOutput [] = return []
    runOutput (unzip -> (lcs, st0:_)) = fst <$> runLocale st0 (output target lcs)

    writeAll :: [(FilePath, T.Text)] -> IO ()
    writeAll = mapM_ (uncurry writeOutput)

    printError e   = printErrorPair (Nothing, e)
    printErrorPair = putStrLn . formatErrorPair

    formatErrorPair (Nothing, e) = "\n*** Error ***\n" ++ show e
    formatErrorPair (Just lc, e) = "\n*** Error in " ++ lcName lc ++ " ***\n"
                                   ++ show e


compileToJava :: FilePath -> FilePath -> String -> IO ()
compileToJava indir outdir package =
    compileGeneric indir
      J.JavaTarget
        { J.javaTabWidth      = 4
        , J.javaExpandTab     = False
        , J.javaDirname       = outdir
        , J.javaPackage       = package
        , J.javaInterfaceName = last $ splitDirectories indir
        }

