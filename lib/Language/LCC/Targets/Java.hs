{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Language.LCC.Targets.Java where

import Prelude hiding (init)

import GHC.Exts (IsList(..))

import Control.Lens hiding ((??))
import Control.Monad (liftM, liftM3, when, forM, zipWithM_)
import Control.Monad.Reader (MonadReader, runReaderT, ask, asks)
import Control.Monad.Writer (MonadWriter, runWriterT, execWriterT, tell)
import Control.Monad.State (MonadState, evalStateT, execStateT, get, put, modify)

import Data.Char (toLower, toUpper)
import Data.Foldable (forM_)
import Data.Function
import Data.List hiding (init)
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

import Language.LCC.Analyzer
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
    ]
  where
    writeClass methods = do
        writeln "private static class _Builtins {"

        indent $
          sequence_ $ intersperse (writeln "") methods

        writeln "}"

    method :: Writing m => T.Text -> T.Text -> [T.Text] -> T.Text -> m ()
    method ret name params expr = do
        let paramList = T.intercalate ", " params
        writefn "private static {} {}({}) {" (ret, name, paramList)
        indent $ writef1 "return {};" expr
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


isConstant :: Translation path ret -> Bool
isConstant = null . _sigParams . _trSig

isMethod :: Translation path ret -> Bool
isMethod = not . isMethod


fmtParam :: Param -> T.Text
fmtParam param = format "{} {}" ( param^.paramType.to javaType
                                , param^.paramName)


constArrayName :: Type -> T.Text
constArrayName t0 = "_constArray_" <> fmt t0
  where
    fmt :: Type -> T.Text
    fmt (TArray t) = "ArrayOf_" <> fmt t
    fmt t          = t^.to show.packed


sigFmtArgs :: PathNode -> AnalyzedTranslation -> (T.Text, T.Text, String, T.Text)
sigFmtArgs name t = (access, ret, name, argList)
  where
    access  = if isPrivateTr t then "private" else "public"
    ret     = t^.trSig.sigReturn.to javaType
    argList = T.intercalate ", " $ map fmtParam (t^.trSig.sigParams)


fmtInputArg :: String -> T.Text
fmtInputArg name = "_in_" <> T.pack name


fmtExpr :: (Err.ErrorM m, ScopedAbs Type m) => AnalyzedAST -> AbsExpr -> m T.Text
fmtExpr ast = \case
    IntL x    -> return $ show x ^. packed
    DoubleL x -> return $ show x ^. packed
    BoolL b   -> return $ if b then "true" else "false"

    CharL ch  -> return $ format1  "'{}'"  (quoteChar ch)
    StringL s -> return $ format1 "\"{}\"" (quoteString s)

    SConcat [] -> return "\"\""
    SConcat s  -> liftM (T.intercalate " + ") $ mapM (fmtExpr ast)  s

    Array arr  -> fmtArray arr

    Cond c t f -> liftCM3 (format "({} ? {} : {})") (fmtExpr ast c)
                                                    (fmtExpr ast t)
                                                    (fmtExpr ast f)

    Funcall (Builtin sig)          args -> fmtBuiltin sig args
    Funcall (Input _ name)         _    -> fmtInputArg name & return
    Funcall (Fn (VParamName name)) _    -> fmtParamName name
    Funcall (Fn (VAbsolutePath p)) args -> fmtFuncall p args
  where
    liftCM3 f x y z = liftM f $ liftM3 (,,) x y z

    fmtArray elems = do
        TArray elemType <- typeOf ast (Array elems)
        elemsFmt'd      <- mapM (fmtExpr ast) elems

        return $ format "new {}[] { {} }" ( elemType^.to javaType
                                          , T.intercalate ", " elemsFmt'd
                                          )

    fmtBuiltin sig args =
        case builtins ^? ix sig of
          Just (sig', subsFn) -> do
            let _ = sig' `asTypeOf` sig

            fmtdArgs <- mapM (fmtExpr ast) args

            return $ format "{}({})" (subsFn, T.intercalate ", " fmtdArgs)

          Nothing ->
            Err.signatureNotFound (sig^.sigPath.absolute.re _Absolute)
                                  (sig^..sigParams.traverse.paramType)

    fmtParamName =
        return . T.pack

    fmtFuncall path args = do
        let expr' = Funcall (Fn (VAbsolutePath path)) args

        Err.panic $
          "fmtFuncall: All function calls should have been inlined, but found "
            <> show expr'


writePreamble :: Writing m => m ()
writePreamble = do
    writef1 "package {};" =<< asks (T.pack . javaPackage)
    writeln ""
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


writeInterfaceConstructor :: Writing m
                          => [Type] -> [T.Text] -> [(PathNode, T.Text)]-> m ()
writeInterfaceConstructor types names extraStmts = do
    writef1 "private {}(" =<< asks javaInterfaceName

    let firstType = head types

    indent . indent $ do
      zipWithM_ (\t name -> do
                   let sep = if t == firstType then ' ' else ','

                   writefn "{} {}[] {}" (sep, javaType t, name))
        types names

      writeln ") {"
      writeln ""

    indent $ do
      forM_ names $ \name ->
        writefn "this.{} = {};" (name, name)

      writeln ""

      forM_ extraStmts $ \(lhs, rhs) ->
        writefn "this.{} = {};" (lhs, rhs)

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
    sort $ nub [tr^.trSig.sigReturn | tr <- ast^..traverse, isConstant tr]


writeInterfaceBody :: (Err.ErrorM m, Writing m)
                   => AnalyzedAST
                   -> m [(PathNode, T.Text)]
writeInterfaceBody ast =
    case ast of
      Leaf trs ->
        Err.globalPanic $ "writeInterfaceBody: Translations at the top level: "
                            ++ show (trs^..traverse.trSig)
      Subtree m ->
        evalStateT (writeTopLevel m) Map.empty
  where
    writeTopLevel :: (Writing m, ConstArrayCountM m)
                  => Map.Map PathNode AnalyzedAST -> m [(PathNode, T.Text)]
    writeTopLevel m = do
        let foreach = forM (Map.toList m) . uncurry

        constructorStatements <-
            foreach $ \name -> \case
                Leaf trs ->
                  liftM concat $
                    forM trs $ \t ->
                      if isConstant t
                        then writeConstValDecl False name t & liftM return
                        else writeAbstractDecl       name t >> return []

                Subtree m' ->
                  writeNestedClassDecl False name m' & liftM (return . fst)

        return (concat constructorStatements)


    writeConstValDecl :: (Writing m, ConstArrayCountM m)
                      => Bool
                      -> PathNode
                      -> AnalyzedTranslation
                      -> m (PathNode, T.Text)
    writeConstValDecl init name t = do
        let ret = t^.trSig.sigReturn
        i <- nextConstArrayIndex ret

        let array = constArrayName ret

        let rhs = format "{}[{}]" (array, i)

        writefn "public final {} {}{};" (javaType ret, name,
            if init then " = " <> rhs else "")

        return (name, rhs)

    writeAbstractDecl :: Writing m => PathNode -> AnalyzedTranslation -> m ()
    writeAbstractDecl name t = do
        writefn "{} abstract {} {}({});" (sigFmtArgs name t)


    writeNestedClassDecl :: (Writing m, ConstArrayCountM m)
                         => Bool
                         -> PathNode
                         -> Map.Map PathNode AnalyzedAST
                         -> m ((PathNode, T.Text), Any)
    writeNestedClassDecl init name m = do
        writef1 "public abstract class {} {" name

        hasAbstractMembers <-
          indent $ foldMapWithTagsM m writeSignature writeNestedClass

        writeln "}"

        rhs <-
          if getAny hasAbstractMembers
            then do
              writefn "protected abstract {} _new_{}();" (name, name)

              return $ format1 "_new_{}()" name
            else
              return $ format "new {}() {}" (name, "{}" :: T.Text)

        writefn "public final {} {}{};" (name, name,
            if init then " = " <> rhs else "")

        writeln ""

        return ((name, rhs), hasAbstractMembers)


    writeSignature :: (Writing m, ConstArrayCountM m)
                   => PathNode -> AnalyzedTranslation -> m Any
    writeSignature name t
      | isConstant t = do
          writeConstValDecl True name t
          return (Any False)

      | otherwise = do
          writeAbstractDecl name t
          return (Any True)

    writeNestedClass :: (Writing m, ConstArrayCountM m)
                     => PathNode
                     -> Map.Map PathNode AnalyzedAST
                     -> m Any
    writeNestedClass name m =
        writeNestedClassDecl True name m & liftM snd


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
writeInstantiation (Locale name inputs ast) = do
    className <- asks javaInterfaceName

    let params = inputs <&> \(LocaleInput (Param t n) _) ->
                     format "final {} {}" (javaType t, fmtInputArg n)

    writefn "public static {} _locale_{}({}) {"
        (className, name, T.intercalate ", " params)

    indent $ do
      writef1 "return new {}(" className

      indent.indent $ do
        writeConstArrayValues =<< collectConstArrayValues ast

        writeln ") {"

      indent $
        writeImplementations ast

      writeln "};"

    writeln "}"
    writeln ""


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

      let constTypes = collectConstArrayTypes sampleAST
      let constNames = map constArrayName constTypes

      writeConstArrayDecls constTypes
      writeln ""

      constructorStatements <- writeInterfaceBody sampleAST
      writeln ""

      writeInterfaceConstructor constTypes constNames constructorStatements
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
        fold builtins $ \_ (builtinSig, _) ->
          addTranslation (builtinTranslation builtinSig)
      where
        fold m f = Map.foldrWithKey' (\p b l' -> l' >>= f p b) (return l) m

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
