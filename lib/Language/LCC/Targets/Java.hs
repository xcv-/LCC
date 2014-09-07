{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Language.LCC.Targets.Java where

import GHC.Exts (IsList(..))

import Control.Lens hiding ((??))
import Control.Monad (liftM, liftM3, when, zipWithM_)
import Control.Monad.Reader (MonadReader, runReaderT, ask, asks)
import Control.Monad.Writer (MonadWriter, runWriterT, execWriterT, tell)
import Control.Monad.State (MonadState, evalStateT, execStateT, get, put, modify)

import Data.Char (toLower, toUpper)
import Data.Foldable (forM_)
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text.Lens
import Data.Text.Format (format)

import qualified Data.DList as DL
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB

import Text.Parsec.Pos

import System.FilePath ((</>))

import Language.LCC.AST
import Language.LCC.Target
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err


type Writing m = (MonadWriter (DL.DList T.Text) m, MonadReader JavaTarget m)
type Snoc' s a = Snoc s s a a


data JavaTarget = JavaTarget
    { javaPackage   :: String
    , javaTabWidth  :: Int
    , javaExpandTab :: Bool
    , javaDirname   :: FilePath
    , javaInterfaceName :: String
    }

instance TargetConfig JavaTarget where
    cfgTabWidth  = javaTabWidth
    cfgExpandTab = javaExpandTab



builtinParams :: [Type] -> [Param]
builtinParams types =
    zipWith (\t i -> Param t $ "arg" ++ show i) types [1::Int,2..]


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


fmtParam :: Param -> T.Text
fmtParam param = format "{} {}" ( param^.paramType.to javaType
                                , param^.paramName)


constArrayName :: Type -> T.Text
constArrayName t0 = "_constArray_" <> fmt t0
  where
    fmt :: Type -> T.Text
    fmt (TArray t) = "ArrayOf_" <> fmt t
    fmt t          = t^.to show.packed


varSigFmtArgs :: PathNode -> AnalyzedTranslation -> Int
              -> (T.Text, T.Text, String, T.Text, Int)
varSigFmtArgs name t i = (access, ret, name, constArray, i)
  where
    (access, ret, _, _) = sigFmtArgs name t
    constArray = constArrayName (t^.trSig.sigReturn)


sigFmtArgs :: PathNode -> AnalyzedTranslation -> (T.Text, T.Text, String, T.Text)
sigFmtArgs name t = (access, ret, name, argList)
  where
    access  = if isPrivateTr t then "private" else "public"
    ret     = t^.trSig.sigReturn.to javaType
    argList = T.intercalate ", " $ map fmtParam (t^.trSig.sigParams)


fmtBuiltin :: (Err.ErrorM m, ScopedAbs Type m)
           => AnalyzedAST -> AnalyzedSignature -> [AbsExpr] -> m T.Text
fmtBuiltin ast sig args =
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
                    (Fn (VParamName name), _   ) -> return (name^.packed)
                    (Fn (VAbsolutePath p), args) -> fmtFuncall p args

  | is _Cond = let (c,t,f) = expr^?!_Cond
               in liftCM3 (format "({} ? {} : {})") (fmtExpr ast c)
                                                    (fmtExpr ast t)
                                                    (fmtExpr ast f)
  | otherwise = error $ "Java target: fmtExpr: unknown expression: " ++ show expr
  where
    is :: Prism' AbsExpr a -> Bool
    is p = has p expr

    liftCM3 f x y z = liftM f $ liftM3 (,,) x y z

    fmtArray elems = do
        TArray elemType <- typeOf ast (Array elems)
        elemsFmt'd      <- mapM (fmtExpr ast) elems

        return $ format "new {}[] { {} }" ( elemType^.to javaType
                                          , T.intercalate ", " elemsFmt'd
                                          )
    fmtFuncall path args = do
        let expr' = Funcall (Fn (VAbsolutePath path)) args

        Err.panic $
          "fmtFuncall: All function calls should have been inlined, but found "
            <> show expr'


writePreamble :: Writing m => m ()
writePreamble = do
    writef1 "package {};" =<< asks (T.pack . javaPackage)
    writeln ""
    writeln "import java.util.Map;"
    writeln "import java.util.HashMap;"
    writeln ""


checkInterfaceConsistency :: Err.ErrorM m => [AnalyzedLocale] -> m ()
checkInterfaceConsistency ls
  | length ls < 2 = return ()
  | otherwise =
      case uncurry ordDiffs =<< pairs (map flattenSigs ls) of
        []      -> return ()
        missing -> Err.missingSignatures missing
  where
    ordDiffs :: (String, [AnalyzedSignature])
             -> (String, [AnalyzedSignature])
             -> [Err.MissingSignature]
    ordDiffs (n1, [])     (n2, ss2)    = map (missingIn n1 n2) ss2
    ordDiffs (n1, ss1)    (n2, [])     = map (missingIn n2 n1) ss1
    ordDiffs (n1, s1:ss1) (n2, s2:ss2)
      | s1 < s2   = missingIn n2 n1 s1 : ordDiffs (n1,    ss1) (n2, s2:ss2)
      | s1 > s2   = missingIn n1 n2 s2 : ordDiffs (n1, s1:ss1) (n2,    ss2)
      | otherwise = ordDiffs (n1, ss1) (n2, ss2)

    missingIn :: String -> String -> AnalyzedSignature -> Err.MissingSignature
    missingIn n1 n2 s = Err.MissingSignature { Err.missingInLocale   = n1
                                             , Err.missingSig        = s
                                             , Err.missingComparedTo = n2
                                             }

    flattenSigs :: AnalyzedLocale -> (String, [AnalyzedSignature])
    flattenSigs l = (l^.localeName, l^..localeAST.traverse.trSig)

    pairs :: [a] -> [(a,a)]
    pairs xs = scanl (\(_,b) c -> (b, c)) (xs!!0, xs!!1) (drop 2 xs)


writeConstArrayDecls :: Writing m => [Type] -> m ()
writeConstArrayDecls types =
    forM_ types $ \t ->
      writefn "private final {}[] {};" (javaType t, constArrayName t)


writeInterfaceConstructor :: Writing m => [Type] -> [T.Text] -> m ()
writeInterfaceConstructor types names = do
    writef1 "private {}(" =<< asks javaInterfaceName

    let firstType = head types

    indent . indent $ do
      zipWithM_ (\t name -> do
                   let sep = if t == firstType then ' ' else ','

                   writefn "{} {}[] {}" (sep, javaType t, name))
        types names

      writeln ") {"

    indent $
      forM_ names $ \name ->
        writefn "this.{} = {};" (name, name)

    writeln "}"
    writeln ""


type ConstArrayCount  = Map.Map Type Int
type ConstArrayCountM = MonadState ConstArrayCount

nextConstArrayIndex :: ConstArrayCountM m => Type -> m Int
nextConstArrayIndex typ = do
    m <- get
    let (Just i, m') = m & at typ <%~ Just . maybe 1 (+1)
    put m'
    return (i-1)


collectConstArrayTypes :: AnalyzedAST -> [Type]
collectConstArrayTypes ast =
    sort $ nub [ sig^.sigReturn | tr <- ast^..traverse, let sig = tr^.trSig
               , null (sig^.sigParams) && isPublicTr tr ]


writeInterfaceBody :: (Err.ErrorM m, Writing m) => AnalyzedAST -> m ()
writeInterfaceBody filteredAst =
    case filteredAst of
      Leaf trs ->
        Err.globalPanic $ "writeInterfaceBody: Translations at the top level: "
                            ++ show (trs^..traverse.trSig)
      Subtree m ->
        evalStateT (foldMapWithTagsM m writeSignature writeNestedClass)
                   Map.empty
          >> return ()
  where
    writeSignature :: (Writing m, ConstArrayCountM m)
                   => PathNode -> AnalyzedTranslation -> m Any
    writeSignature name t
      | null $ t^.trSig.sigParams = do
          i <- nextConstArrayIndex $ t^.trSig.sigReturn
          writefn "{} final {} {} = {}[{}];" (varSigFmtArgs name t i)
          writeln ""
          return (Any False)

      | otherwise = do
          writefn "{} abstract {} {}({});" (sigFmtArgs name t)
          writeln ""
          return (Any True)

    writeNestedClass :: (Writing m, ConstArrayCountM m)
                     => PathNode -> Map.Map PathNode AnalyzedAST -> m Any
    writeNestedClass name m = do
        writef1 "public abstract class {} {" name

        hasAbstractMembers <-
          indent $ foldMapWithTagsM m writeSignature writeNestedClass

        writeln "}"

        new <-
          if getAny hasAbstractMembers
            then do
              writefn "protected abstract {} _new_{}();" (name, name)

              return $ format1 "_new_{}()" name

            else
              return $ format "new {}() {}" (name, "{}" :: T.Text)

        writefn "public final {} {} = {};" (name, name, new)
        writeln ""

        return hasAbstractMembers




type ConstArrayValues = Map.Map Type (DL.DList (AbsolutePath, T.Text))
type ConstArrayCollectM = MonadState ConstArrayValues


collectConstArrayValues :: Err.ErrorM m => AnalyzedAST -> m ConstArrayValues
collectConstArrayValues ast =
    flip execStateT Map.empty $
      forM_ ast $ \t -> do
        let sig = t^.trSig

        when (sig^.sigParams.to null && isPublicTr t) $ do
          impl <- t ./> fmtExpr ast . view trImpl

          store (sig^.sigReturn) (sig^.sigPath) impl
  where
    store :: ConstArrayCollectM m => Type -> AbsolutePath -> T.Text -> m ()
    store ty path impl =
      modify $ at ty %~ Just . \case
        Just vals -> DL.snoc vals (path, impl)
        Nothing   -> DL.singleton (path, impl)


writeImplementations :: (Err.ErrorM m, Writing m) => AnalyzedAST -> m ()
writeImplementations ast =
    case ast of
      Leaf trs ->
        Err.globalPanic $ "writeImplementations: Translation at the top level: "
                            ++ show (trs^..traverse.trSig)
      Subtree m -> do
        foldMapWithTagsM m writeMethod writeNestedClass
        return ()
  where
    writeMethod :: (Err.ErrorM m, Writing m)
                => PathNode -> AnalyzedTranslation -> m Any
    writeMethod name t
      | null $ t^.trSig.sigParams =
          return (Any False)

      | otherwise = do
          writeln "@Override"
          writefn "{} final {} {}({}) {" (sigFmtArgs name t)

          indent $
            writef1 "return {};" =<< (t ./> fmtExpr ast . view trImpl)

          writeln "}"
          writeln ""

          return (Any True)

    writeNestedClass :: (Err.ErrorM m, Writing m)
                     => PathNode -> Map.Map PathNode AnalyzedAST -> m Any
    writeNestedClass name m = do
        let write :: (Err.ErrorM m, Writing m) => m Any
            write = do
              writeln "@Override"
              writefn "protected final {} _new_{}() {" (name, name)

              indent $
                writef1 "return new {}() {" name

              hasOverrides <-
                indent.indent $
                  foldMapWithTagsM m writeMethod writeNestedClass

              indent $
                writeln "};"

              writeln "}"
              writeln ""

              return hasOverrides

        cfg <- ask
        case runReaderT (runWriterT write) cfg of
          Right (Any hasOverrides, w)
            | hasOverrides -> tell w >> return (Any True)
            | otherwise    -> return (Any False)

          Left e -> Err.rethrow e



writeConstArrayValues :: Writing m => ConstArrayValues -> m ()
writeConstArrayValues arrays = do
    let firstType = fst $ Map.findMin arrays

    iforM_ arrays $ \ty values -> do
      let sep = if ty == firstType then ' ' else ','

      writefn "{} new {}[] {" (sep, javaType ty)

      indent $
        forM_ values $ \(path, impl) -> do
          writef1 "/* {} */" (show path)
          writef1 "{},"      impl

      writeln "}"


writeInstantiation :: (Err.ErrorM m, Writing m) => AnalyzedLocale -> m ()
writeInstantiation l = do
    className <- asks javaInterfaceName

    let (name, ast) = (l^.localeName, l^.localeAST)

    writefn "public final {} {} = new {}(" (className, name, className)


    indent.indent $ do
      writeConstArrayValues =<< collectConstArrayValues ast

      writeln ") {"

    indent $
      writeImplementations ast

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
      let sampleAST = head ls ^. localeAST

      let constArrayTypes = collectConstArrayTypes sampleAST
      let constArrayNames = map constArrayName constArrayTypes

      writeConstArrayDecls constArrayTypes
      writeln ""

      writeInterfaceConstructor constArrayTypes constArrayNames
      writeln ""

      writeInterfaceBody sampleAST
      writeln ""

      forM_ ls $ \l -> do
        writeInstantiation l
        writeln ""

      writeLocalesArray ls
      writeln ""
      writeBuiltinsClass

    writeln "}"



instance Target JavaTarget where
    injectBuiltins _ l =
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
        fold m f = Map.foldrWithKey' (\p b l' -> l' >>= f p b) (return l) m

        insertNotConflicting :: (Err.ErrorM m, Show ret)
                             => AbsTranslation ret
                             -> Maybe [AbsTranslation ret]
                             -> m [AbsTranslation ret]
        insertNotConflicting builtin prevs =
            case find (matchTrParams builtin) =<< prevs of
              Nothing    -> return $ builtin : concat (maybeToList prevs)
              Just confl -> Err.conflict [confl, builtin]

    output t ls = do
        let ls' = map (localeAST %~ sortOverloads . filterTree isPublicTr) ls

        checkInterfaceConsistency ls'

        classLines <- flip runReaderT t . execWriterT $ writeLocales ls'

        let builders = map TB.fromLazyText . intersperse "\n" $ toList classLines
            content  = TB.toLazyText (mconcat builders)

        return [(file, content)]
      where
        file        = javaDirname t </> packagePath </> fileName
        packagePath = replace '.' '/' (javaPackage t)
        fileName    = javaInterfaceName t ++ ".java"

        replace what with = map (\x -> if x == what then with else x)

        sortOverloads :: AnalyzedAST -> AnalyzedAST
        sortOverloads = \case
            Leaf ts   -> Leaf $ sortBy (compare `on` _trSig) ts
            Subtree m -> Subtree (m & traverse %~ sortOverloads)


    inlineBuiltin _ sig args = return $
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
              [BoolL   b] -> StringL $ show b & _head %~ toLower
              [CharL   c] -> StringL $ show c
              [StringL s] -> StringL $ show s
              _           -> noop

          _ -> noop
      where
        noop :: AbsExpr
        noop = Funcall (Builtin sig) args

        (??) :: a -> a -> Bool -> a
        (??) t f c = if c then t else f
