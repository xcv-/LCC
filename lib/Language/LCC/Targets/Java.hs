{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Language.LCC.Targets.Java
  ( JavaTarget (..)
  ) where

import GHC.Exts (IsList(..))

import Control.Lens
import Control.Monad (liftM, liftM2, liftM3)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Control.Monad.Writer (MonadWriter, execWriterT, tell, censor)

import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text.Format
import Data.Text.Lens
import Data.Text.Buildable as Buildable
import Data.Text.Format.Params

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB

import Text.Parsec.Pos

import System.FilePath ((</>))

import Language.LCC.Target
import Language.LCC.Types
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err


data JavaTarget = JavaTarget
    { javaPackage   :: String
    , javaTabWidth  :: Int
    , javaExpandTab :: Bool
    , javaDirname   :: FilePath
    , javaInterfaceName :: String
    }


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
    mempty = DiffList id
    mappend f g = DiffList $ getDiffList f . getDiffList g


dfmap :: (a -> a) -> DiffList a -> DiffList a
dfmap f (DiffList g) = DiffList $ fmap f . g

instance IsList (DiffList a) where
    type Item (DiffList a) = a
    fromList = DiffList . const
    toList dl = getDiffList dl []


type Writing m = (MonadWriter (DiffList T.Text) m, MonadReader JavaTarget m)


format1 :: Buildable b => Format -> b -> T.Text
format1 fmt = format fmt . Only

writeln :: MonadWriter (DiffList a) m => a -> m ()
writeln l = tell [l]

writef1 :: (MonadWriter (DiffList T.Text) m, Buildable b) => Format -> b -> m ()
writef1 fmt = writeln . format1 fmt

writefn :: (MonadWriter (DiffList T.Text) m, Params ps) => Format -> ps -> m ()
writefn fmt = writeln . format fmt



indent :: Writing m => m a -> m a
indent m = do
    unit <- munit
    flip censor m . dfmap $ \line ->
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


builtinParam :: Type -> Param
builtinParam pType = Param pType "<?>"


builtinTranslation :: AbsSignature Type -> Translation path UnknownType
builtinTranslation sig = Translation
                           { _trSig       = sig & sigReturn .~ UnknownType
                           , _trImpl      = Builtin sig
                           , _trSourcePos = newPos "<builtin>" 0 0
                           }

builtins :: (Ord path1, FromAbsolute path1, FromAbsolute path2)
         => Map.Map path1 (Signature path2 Type, T.Text)
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
    , (["lower"], [TChar],   TChar,   "_Builtins.toLowerCase")

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
                            , _sigParams = map builtinParam paramTypes
                            , _sigReturn = ret
                            }
        in (_Absolute # path, (sig, replacement))


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
        writeln "public static class _Builtins {"

        indent $ do
            writeln $ "private static Map<String, Object> dynamics ="
                   <> "new HashMap<String, Object>();"

            writeln ""
            sequence_ methods

        writeln "}"

    method :: Writing m => T.Text -> T.Text -> [T.Text] -> T.Text -> m ()
    method ret name params expr = do
        let paramList = T.intercalate ", " params
        writefn "public static {} {}({}) {" (ret, name, paramList)
        indent $ writef1 "return {};" expr
        writeln "}"

    getDynamic ret =
        method ret ("getDynamic" <> boxed ret) ["String name"] $
            format1 "({}) dynamics.get(name)" ret

    hasDynamic ret =
        method "boolean" ("hasDynamic" <> boxed ret) ["String name"] $
            "dynamics.get(name) instanceof " <> boxed ret

    addDynamic = do
        writeln "public static void addDynamic(String name, Object value) {"
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


removePrefix :: (IsList l, Eq (Item l)) => l -> l -> l
removePrefix xs ys = fromList . fst . unzip $ dropWhile eq zipped
  where
    zipped = zip (toList xs) (toList ys)
    eq = uncurry (==)


fmtParam :: Param -> T.Text
fmtParam param = format "{} {}" ( param^.paramType.to javaType
                                , param^.paramName)


sigFmtArgs :: PathNode -> AbsSignature Type -> (T.Text, String, T.Text)
sigFmtArgs name sig = (ret, name, argList)
  where
    ret     = sig^.sigReturn.to javaType
    argList = T.intercalate ", " $ map fmtParam (sig^.sigParams)


fmtExpr :: (Err.ErrorM m, ScopedAbs Type m)
        => AbsAST Type -> AbsExpr -> m T.Text
fmtExpr ast expr
  | is _IntL    = return $ expr^?!_IntL.to show.packed
  | is _DoubleL = return $ expr^?!_DoubleL.to show.packed
  | is _BoolL   = return $ expr^?!_BoolL.to (\case {True->"true"; _->"false"})
  | is _CharL   = return . format1  "'{}'"  . quoteChar   $ expr^?!_CharL
  | is _StringL = return . format1 "\"{}\"" . quoteString $ expr^?!_StringL
  | is _Array   = fmtArray $ expr^?!_Array
  | is _SConcat = liftM (T.intercalate " + ") $ case expr^?!_SConcat of
                                                  [] -> return ["\"\""]
                                                  xs -> mapM (fmtExpr ast) xs
  | is _Funcall = case expr^?!_Funcall of
                    (VParamName name, args) -> return (name^.packed)
                    (VAbsolutePath p, args) -> fmtFuncall p args

  | is _Cond    = let (c,t,f) = expr^?!_Cond
                  in liftCM3 (format "({} ? {} : {})") (fmtExpr ast c)
                                                       (fmtExpr ast t)
                                                       (fmtExpr ast f)
  | is _Builtin = return $ format1 "<builtin: {}>" (show expr)
  where
    is :: Prism' AbsExpr a -> Bool
    is prism = has prism expr

    liftCM3 f x y z = liftM f $ liftM3 (,,) x y z

    fmtArray elems = do
        TArray elemType <- typeOf (Array elems) ast
        elemsFmt'd      <- mapM (fmtExpr ast) elems

        return $ format "new {} { {} }" ( elemType^.to javaType
                                        , T.intercalate ", " elemsFmt'd)

    fmtFuncall path args = do
        relPath   <- liftM (removePrefix path) (viewS $ trSig.sigPath.absolute)
        argsFmt'd <- mapM (fmtExpr ast) args

        return $ format "{}({})" ( show relPath
                                 , T.intercalate ", " argsFmt'd)



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
                   => PathNode -> AbsTranslation Type -> m ()
    writeSignature name Translation {..} = do
        writef1 "// {}" name
        writefn "public abstract {} {}({});" (sigFmtArgs name _trSig)
        writeln ""

    writeNestedClass :: Writing m
                     => PathNode -> Map.Map PathNode (AbsAST Type) -> m ()
    writeNestedClass name m = do
        writef1 "// {}" name
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
                => PathNode -> AbsTranslation Type -> m ()
    writeMethod name t@Translation {..} = do
        writef1 "@Override // {}" name
        writefn "public final {} {}({}) {" (sigFmtArgs name _trSig)
        indent $ writef1 "return {};" =<< (t /> fmtExpr ast _trImpl)
        writeln "}"
        writeln ""

    writeNestedClass :: (Err.ErrorM m, Writing m)
                     => PathNode -> Map.Map PathNode (AbsAST Type) -> m ()
    writeNestedClass name m = do
        writefn "// {}" name
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
    let strings = map (format "\"{}\"" . view localeName) $ ls

    writef1 "public final {} _locales[] = {" =<< asks javaInterfaceName
    indent $ writeln $ T.intercalate ", " strings
    writeln "};"


writeLocales :: (Err.ErrorM m, Writing m) => [AnalyzedLocale] -> m ()
writeLocales [] = Err.globalPanic "writeLocales: Passed 0 locales"
writeLocales ls = do
    writePreamble

    writef1 "public abstract class {} {" =<< asks javaInterfaceName

    indent $ do
      writeInterface (head ls^.localeAST)

      mapM_ writeInstantiation ls

      writeLocalesArray ls
      writeBuiltinsClass

    writeln "}"


instance Target JavaTarget where
    injectBuiltins t l =
        fold builtins $ \path (builtinSig, _) -> do
          let builtin = builtinTranslation builtinSig

          localeAST.atPath path %%~ \case
            Just (Subtree _) ->
              Err.globalPanic $
                "Cannot insert builtin: subtree exists at the same path: "
                  ++ show path

            node ->
              fmap Leaf <$> insertNotConflicting builtin (node^?_Just._Leaf)
      where
        fold m f = Map.foldrWithKey' (\p b l -> l >>= f p b) (return l) m

        insertNotConflicting :: (Err.ErrorM m, Show ret)
                             => AbsTranslation ret
                             -> Maybe [AbsTranslation ret]
                             -> m (Maybe [AbsTranslation ret])
        insertNotConflicting builtin prevs =
            case find (matchTrParams builtin) =<< prevs of
              Nothing    -> return $ Just [builtin] <> prevs
              Just confl -> Err.conflict [confl, builtin]

    output t ls = do
        lines <- flip runReaderT t . execWriterT $ writeLocales ls
        let builder = foldr mappend "" . map TB.fromLazyText $ toList lines
        let content = TB.toLazyText builder

        return [(file, content)]
      where
        file        = javaDirname t </> packagePath </> fileName
        packagePath = replace '.' '/' (javaPackage t)
        fileName    = javaInterfaceName t ++ ".java"

        replace what with = map (\x -> if x == what then with else x)
