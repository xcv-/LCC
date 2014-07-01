{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Language.LCC.Targets.Java
  ( JavaTarget (..)
  ) where

import GHC.Exts (IsList(..))

import Control.Lens
import Control.Monad (liftM, liftM2, liftM3, forM_)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Control.Monad.Writer (MonadWriter, execWriterT, tell, censor)

import Data.Char (toLower, toUpper)
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text.Format
import Data.Text.Lens
import Data.Text.Buildable as Buildable
import Data.Text.Format.Params

import qualified Data.DList as DL
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB

import Text.Parsec.Pos

import System.FilePath ((</>))

import Language.LCC.AST
import Language.LCC.Target
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err


data JavaTarget = JavaTarget
    { javaPackage   :: String
    , javaTabWidth  :: Int
    , javaExpandTab :: Bool
    , javaDirname   :: FilePath
    , javaInterfaceName :: String
    }


instance IsList (DL.DList a) where
    type Item (DL.DList a) = a
    fromList = DL.fromList
    toList = DL.toList


type Writing m = (MonadWriter (DL.DList T.Text) m, MonadReader JavaTarget m)
type Snoc' s a = Snoc s s a a


format1 :: Buildable b => Format -> b -> T.Text
format1 fmt = format fmt . Only

writeln :: MonadWriter (DL.DList a) m => a -> m ()
writeln l = tell [l]

writef1 :: (MonadWriter (DL.DList T.Text) m, Buildable b) => Format -> b -> m ()
writef1 fmt = writeln . format1 fmt

writefn :: (MonadWriter (DL.DList T.Text) m, Params ps) => Format -> ps -> m ()
writefn fmt = writeln . format fmt



indent :: Writing m => m a -> m a
indent m = do
    unit <- munit
    flip censor m . fmap $ \line ->
      if line == mempty
        then line
        else unit <> line
  where
    munit :: MonadReader JavaTarget m => m T.Text
    munit = do
      et <- asks javaExpandTab

      if not et
        then return "\t"
        else do
          tw <- asks (fromIntegral . javaTabWidth)
          return (T.replicate tw " ")


builtinParams :: [Type] -> [Param]
builtinParams types =
    zipWith (\t i -> Param t $ "arg" ++ show i) types [1,2..]


builtinTranslation :: FromParamName path
                   => AbsSignature Type
                   -> Translation path UnknownType
builtinTranslation sig =
    Translation
      { _trSig         = sig & sigReturn .~ UnknownType
      , _trImpl        = Funcall (Builtin sig) args
      , _trAnnotations = [PrivateBuiltin]
      , _trSourcePos   = newPos "<builtin>" 0 0
      }
  where
    args :: FromParamName path => [Expr path]
    args = map (\pName -> Funcall (Fn $ _ParamName # pName) [])
               (sig^..sigParams.traverse.paramName)

builtins :: FromAbsolute path
         => Map.Map AnalyzedSignature (Signature path Type, T.Text)
builtins = Map.fromList $ builtinMap
    [ (["str"], [TChar  ], TString, "Character.toString")
    , (["str"], [TString], TString, ""                  )
    , (["str"], [TInt   ], TString, "Integer.toString"  )
    , (["str"], [TDouble], TString, "Double.toString"   )
    , (["str"], [TBool  ], TString, "Boolean.toString"  )

    , (["eq"], [TChar,   TChar],   TBool, "_Builtins.eq")
    , (["eq"], [TString, TString], TBool, "_Builtins.eq")
    , (["eq"], [TInt,    TInt],    TBool, "_Builtins.eq")
    , (["eq"], [TDouble, TDouble], TBool, "_Builtins.eq")
    , (["eq"], [TBool,   TBool],   TBool, "_Builtins.eq")

    , (["upper"], [TChar],   TChar,   "Character.toUpperCase")
    , (["lower"], [TChar],   TChar,   "Character.toLowerCase")

    , (["upper"], [TString], TString, "_Builtins.toUpper"   )
    , (["lower"], [TString], TString, "_Builtins.toLower"   )

    , (["capitalize"], [TString], TString, "_Builtins.capitalize")

    , (["dynamicChar"],    [TString], TChar,   "_Builtins.getDynamicCharacter")
    , (["dynamicString"],  [TString], TString, "_Builtins.getDynamicString"   )
    , (["dynamicInt"],     [TString], TInt,    "_Builtins.getDynamicInteger"  )
    , (["dynamicDouble"],  [TString], TDouble, "_Builtins.getDynamicDouble"   )
    , (["dynamicBoolean"], [TString], TBool,   "_Builtins.getDynamicBoolean"  )

    , (["hasDynamicChar"],    [TString], TBool, "_Builtins.hasDynamicCharacter")
    , (["hasDynamicString"],  [TString], TBool, "_Builtins.hasDynamicString"   )
    , (["hasDynamicInt"],     [TString], TBool, "_Builtins.hasDynamicInteger"  )
    , (["hasDynamicDouble"],  [TString], TBool, "_Builtins.hasDynamicDouble"   )
    , (["hasDynamicBoolean"], [TString], TBool, "_Builtins.hasDynamicBoolean"  )
    ]
  where
    builtinMap = map $ \(path, paramTypes, ret, replacement) ->
        let sig = Signature { _sigPath   = _Absolute # path
                            , _sigParams = builtinParams paramTypes
                            , _sigReturn = ret
                            }
        in (sig & sigPath .~ _Absolute # path, (sig, replacement))


writeBuiltinsClass :: Writing m => m ()
writeBuiltinsClass = writeClass
    [ method "boolean" "eq"["String s1",  "String s2" ] "s1.equals(s2)"
    , method "boolean" "eq"["char c1",    "char c2"   ] "c1 == c2"
    , method "boolean" "eq"["int x1",     "int x2"    ] "x1 == x2"
    , method "boolean" "eq"["double d1",  "double d2" ] "d1 == d2"
    , method "boolean" "eq"["boolean b1", "boolean b2"] "b1 == b2"

    , method "String" "toUpper"["String s"] "s.toUpperCase()"
    , method "String" "toLower"["String s"] "s.toLowerCase()"

    , method "String" "capitalize" ["String s"]
        "s.length() < 1? s : Character.toUpperCase(s.charAt(0)) + s.substring(1)"

    , getDynamic "String"
    , getDynamic "char"
    , getDynamic "int"
    , getDynamic "double"
    , getDynamic "boolean"

    , hasDynamic "String"
    , hasDynamic "char"
    , hasDynamic "int"
    , hasDynamic "double"
    , hasDynamic "boolean"

    , addDynamic
    ]
  where
    writeClass methods = do
        writeln "private static class _Builtins {"

        indent $ do
            writeln $ "private static Map<String, Object> dynamics = "
                   <> "new HashMap<String, Object>();"

            writeln ""
            sequence_ $ intersperse (writeln "") methods

        writeln "}"

    method :: Writing m => T.Text -> T.Text -> [T.Text] -> T.Text -> m ()
    method ret name params expr = do
        let paramList = T.intercalate ", " params
        writefn "private static {} {}({}) {" (ret, name, paramList)
        indent $ writef1 "return {};" expr
        writeln "}"

    getDynamic ret =
        method ret ("getDynamic" <> boxed ret) ["String name"] $
            format1 "({}) dynamics.get(name)" ret

    hasDynamic ret =
        method "boolean" ("hasDynamic" <> boxed ret) ["String name"] $
            "dynamics.get(name) instanceof " <> boxed ret

    addDynamic = do
        writeln "private static void addDynamic(String name, Object value) {"
        indent $ writeln "dynamics.put(name, value);"
        writeln "}"


javaType :: Type -> T.Text
javaType TInt    = "int"
javaType TDouble = "double"
javaType TBool   = "boolean"
javaType TChar   = "char"
javaType TString = "String"
javaType (TArray e) = javaType e <> "[]"

boxed :: T.Text -> T.Text
boxed t =
  case t of
    "boolean" -> "Boolean"
    "byte"    -> "Byte"
    "char"    -> "Character"
    "float"   -> "Float"
    "double"  -> "Double"
    "int"     -> "Integer"
    "long"    -> "Long"
    "short"   -> "Short"
    other     -> other


getter :: T.Text -> T.Text
getter name = "_get_" <> name

lazy :: T.Text -> T.Text
lazy name = "_lazy_" <> name

constructor :: T.Text -> T.Text
constructor name = "_new_" <> name

call :: (T.Text -> T.Text) -> T.Text -> T.Text
call modifier name = modifier name <> "()"


quote :: Char -> T.Text
quote '\n' = "\\n"
quote '\r' = "\\r"
quote '\t' = "\\t"
quote '\\' = "\\\\"
quote c = T.singleton c

quoteChar :: Char -> T.Text
quoteChar '\'' = "\\'"
quoteChar c = quote c

quoteStringChar :: Char -> T.Text
quoteStringChar '"' = "\\\""
quoteStringChar c = quote c

quoteString :: String -> T.Text
quoteString = mconcat . map quoteStringChar


removePrefix :: (Eq l, IsList l, Eq (Item l), Snoc' l (Item l)) => l -> l -> l
removePrefix xs ys
  | xs == ys  = fromList [xs^?!_last]
  | otherwise = fromList . fst . unzip $ dropWhile eq zipped
  where
    zipped = zip (toList xs) (toList ys)
    eq = uncurry (==)


fmtParam :: Param -> T.Text
fmtParam param = format "{} {}" ( param^.paramType.to javaType
                                , param^.paramName)


sigFmtArgs :: PathNode -> AnalyzedTranslation -> (T.Text, T.Text, String, T.Text)
sigFmtArgs name t = (access, ret, name, argList)
  where
    access  = if isPrivateTr t then "private" else "public"
    ret     = t^.trSig.sigReturn.to javaType
    argList = T.intercalate ", " $ map fmtParam (t^.trSig.sigParams)


fmtBuiltin :: (Err.ErrorM m, ScopedAbs Type m)
           => AnalyzedAST -> AnalyzedSignature -> [AbsExpr] -> m T.Text
fmtBuiltin ast sig args = do
    case builtins^?ix sig of
      Just (sig', subsFn) -> do
        let _ = sig' `asTypeOf` sig

        fmtdArgs <- mapM (fmtExpr ast) args

        return $ format "{}({})" (subsFn, T.intercalate ", " fmtdArgs)

      Nothing ->
        Err.signatureNotFound (sig^.sigPath.absolute.re _Absolute)
                              (sig^..sigParams.traverse.paramType)


fmtExpr :: (Err.ErrorM m, ScopedAbs Type m) => AnalyzedAST -> AbsExpr -> m T.Text
fmtExpr ast expr
  | is _IntL    = return $ expr^?!_IntL.to show.packed
  | is _DoubleL = return $ expr^?!_DoubleL.to show.packed
  | is _BoolL   = return $ expr^?!_BoolL.to (\case {True->"true";_->"false"})
  | is _CharL   = return . format1  "'{}'"  . quoteChar   $ expr^?!_CharL
  | is _StringL = return . format1 "\"{}\"" . quoteString $ expr^?!_StringL
  | is _Array   = fmtArray $ expr^?!_Array
  | is _SConcat = liftM (T.intercalate " + ") $ case expr^?!_SConcat of
                                                  [] -> return ["\"\""]
                                                  xs -> mapM (fmtExpr ast) xs
  | is _Funcall = case expr^?!_Funcall of
                    (Builtin sig,          args) -> fmtBuiltin ast sig args
                    (Fn (VParamName name), args) -> return (name^.packed)
                    (Fn (VAbsolutePath p), args) -> fmtFuncall p args

  | is _Cond = let (c,t,f) = expr^?!_Cond
               in liftCM3 (format "({} ? {} : {})") (fmtExpr ast c)
                                                    (fmtExpr ast t)
                                                    (fmtExpr ast f)
  where
    is :: Prism' AbsExpr a -> Bool
    is prism = has prism expr

    liftCM3 f x y z = liftM f $ liftM3 (,,) x y z

    fmtArray elems = do
        TArray elemType <- typeOf ast (Array elems)
        elemsFmt'd      <- mapM (fmtExpr ast) elems

        return $ format "new {}[] { {} }" ( elemType^.to javaType
                                          , T.intercalate ", " elemsFmt'd
                                          )
    fmtFuncall path args = do
        relPath  <- liftM (removePrefix path) (viewS $ trSig.sigPath.absolute)
        fmtdArgs <- mapM (fmtExpr ast) args

        return $ format "{}({})" ( show (relPath^.from absolute)
                                 , T.intercalate ", " fmtdArgs
                                 )


writePreamble :: Writing m => m ()
writePreamble = do
    writef1 "package {};" =<< asks (T.pack . javaPackage)
    writeln ""
    writeln "import java.util.Map;"
    writeln "import java.util.HashMap;"
    writeln ""


writeInterface :: (Err.ErrorM m, Writing m) => AbsAST Type -> m ()
writeInterface ast =
    case ast of
      Leaf trs ->
        Err.globalPanic $ "writeInterface: Translations at the top level: "
                            ++ show (trs^..traverse.trSig)
      Subtree m ->
        mapWithTagsM_ m writeSignature writeNestedClass
  where
    writeSignature :: Writing m
                   => PathNode -> AnalyzedTranslation -> m ()
    writeSignature name t = do
        writefn "{} abstract {} {}({});" (sigFmtArgs name t)
        writeln ""

    writeNestedClass :: Writing m
                     => PathNode -> Map.Map PathNode (AbsAST Type) -> m ()
    writeNestedClass name m = do
        writef1 "public abstract class _Abstract_{} {" name
        indent $ mapWithTagsM_ m writeSignature writeNestedClass
        writeln "}"
        writeln ""


writeImplementation :: (Err.ErrorM m, Writing m) => AbsAST Type -> m ()
writeImplementation ast =
    case ast of
      Leaf trs ->
        Err.globalPanic $ "writeImplementation: Translation at the top level: "
                            ++ show (trs^..traverse.trSig)
      Subtree m -> mapWithTagsM_ m writeMethod writeNestedClass
  where
    writeMethod :: (Err.ErrorM m, Writing m)
                => PathNode -> AnalyzedTranslation -> m ()
    writeMethod name t = do
        writeln "@Override"
        writefn "{} final {} {}({}) {" (sigFmtArgs name t)
        indent $ writef1 "return {};" =<< (t ./> fmtExpr ast . view trImpl)
        writeln "}"
        writeln ""

    writeNestedClass :: (Err.ErrorM m, Writing m)
                     => PathNode -> Map.Map PathNode (AbsAST Type) -> m ()
    writeNestedClass name m = do
        writefn "public final class _Impl_{} extends _Abstract_{} {" (name,name)
        indent $ mapWithTagsM_ m writeMethod writeNestedClass
        writeln "}"
        writeln ""


writeInstantiation :: (Err.ErrorM m, Writing m) => AnalyzedLocale -> m ()
writeInstantiation l = do
    className <- asks javaInterfaceName
    let name = l^.localeName

    writefn "public final {} {} = new {}() {" (className, name, className)
    indent $ writeImplementation (l^.localeAST)
    writeln "};"


writeLocalesArray :: Writing m => [AnalyzedLocale] -> m ()
writeLocalesArray ls = do
    let strings = map (format1 "\"{}\"" . view localeName) ls

    writeln "public final String _locales[] = {"
    indent $ writeln $ T.intercalate ", " strings
    writeln "};"


writeLocales :: (Err.ErrorM m, Writing m) => [AnalyzedLocale] -> m ()
writeLocales [] = Err.globalPanic "writeLocales: Passed 0 locales"
writeLocales ls = do
    writePreamble

    writef1 "public abstract class {} {" =<< asks javaInterfaceName

    indent $ do
      writeInterface (head ls^.localeAST)
      writeln ""

      forM_ ls $ \l -> do
        writeInstantiation l
        writeln ""

      writeLocalesArray ls
      writeln ""
      writeBuiltinsClass

    writeln "}"


instance Target JavaTarget where
    injectBuiltins t l =
        fold builtins $ \sig (builtinSig, _) -> do
          let builtin = builtinTranslation builtinSig

          localeAST.atPath (sig^.sigPath) %%~ \case
            Just (Subtree _) ->
              Err.globalPanic $
                "Cannot insert builtin: subtree exists at the same path: "
                  ++ show (sig^.sigPath)

            node ->
              Just . Leaf <$> insertNotConflicting builtin (node^?_Just._Leaf)
      where
        fold m f = Map.foldrWithKey' (\p b l -> l >>= f p b) (return l) m

        insertNotConflicting :: (Err.ErrorM m, Show ret)
                             => AbsTranslation ret
                             -> Maybe [AbsTranslation ret]
                             -> m [AbsTranslation ret]
        insertNotConflicting builtin prevs =
            case find (matchTrParams builtin) =<< prevs of
              Nothing    -> return $ builtin : concat (maybeToList prevs)
              Just confl -> Err.conflict [confl, builtin]

    output t ls = do
        lines <- flip runReaderT t . execWriterT $ writeLocales ls

        let builders = map TB.fromLazyText . intersperse "\n" $ toList lines
            content  = TB.toLazyText (mconcat builders)

        return [(file, content)]
      where
        file        = javaDirname t </> packagePath </> fileName
        packagePath = replace '.' '/' (javaPackage t)
        fileName    = javaInterfaceName t ++ ".java"

        replace what with = map (\x -> if x == what then with else x)


    inlineBuiltin t sig args = return $
        case sig^.sigPath of
          ["capitalize"] ->
            case args of
              [StringL s] -> StringL (over _head toUpper s)
              _           -> noop

          ["eq"]
            | all isLiteral args -> BoolL . isJust $
                foldl1' (\acc x -> acc ?? Nothing $ acc == x) (map Just args)
            | otherwise          -> noop

          ["lower"] ->
            case args of
              [CharL   c] -> CharL   (toLower c)
              [StringL s] -> StringL (map toLower s)
              _           -> noop

          ["upper"] ->
            case args of
              [CharL   c] -> CharL   (toUpper c)
              [StringL s] -> StringL (map toUpper s)
              _           -> noop

          ["str"] ->
            case args of
              [IntL    x] -> StringL $ show x
              [DoubleL x] -> StringL $ show x
              [BoolL   b] -> StringL $ show b
              [CharL   c] -> StringL $ show c
              [StringL s] -> StringL $ show s
              _           -> noop

          _ -> noop
      where
        noop :: AbsExpr
        noop = Funcall (Builtin sig) args

        (??) :: a -> a -> Bool -> a
        (??) t f c = if c then t else f
