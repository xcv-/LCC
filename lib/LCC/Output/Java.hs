{-# LANGUAGE OverloadedStrings #-}
module LCC.Output.Java
  ( JavaTarget (..)
  ) where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid ((<>))

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad
import Control.Monad.Error
import Control.Applicative

import System.IO
import System.FilePath ((</>))

import LCC.Analyzer
import LCC.State
import LCC.Target
import LCC.Types


data JavaTarget = JavaTarget
    { javaPackage :: String
    , javaTabWidth :: Int
    , javaExpandTab :: Bool
    , javaDirname :: FilePath
    , javaInterfaceName :: String
    }


builtinParam :: Type -> Param
builtinParam pType = Param pType "<?>"

builtins :: Map.Map TranslationSignature T.Text
builtins = Map.fromList $ builtinMap
    [ (["str"], [TChar  ], TString, "Character.toString")
    , (["str"], [TString], TString, ""                  )
    , (["str"], [TInt   ], TString, "Integer.toString"  )
    , (["str"], [TDouble], TString, "Double.toString"   )
    , (["str"], [TBool  ], TString, "Boolean.toString"  )
    ]
  where
    builtinMap = map $ \(path, paramTypes, ret, replacement) ->
        ( Signature { sigPath = AbsVarPath path
                    , sigParams = map builtinParam paramTypes
                    , sigReturn = ret
                    }
        , replacement
        )

isBuiltin :: AbsVarPath -> [Type] -> Bool
isBuiltin path paramTypes =
    isJust $ find match (Map.keys builtins)
  where
    match :: TranslationSignature -> Bool
    match signature = sigPath signature   == path
                   && sigParams signature == params
    params :: [Param]
    params = map builtinParam paramTypes



javaIndent :: JavaTarget -> Int -> T.Text
javaIndent target count = T.replicate count unit
  where
    unit = if javaExpandTab target
             then T.replicate (javaTabWidth target) " "
             else "\t"

javaType :: Type -> T.Text
javaType TAny    = error "TAny passed to javaType"
javaType TInt    = "int"
javaType TDouble = "double"
javaType TBool   = "boolean"
javaType TChar   = "char"
javaType TString = "String"
javaType (TArray e) = javaType e <> "[]"

javaBoxed :: T.Text -> T.Text
javaBoxed "boolean" = "Boolean"
javaBoxed "byte"    = "Byte"
javaBoxed "char"    = "Character"
javaBoxed "float"   = "Float"
javaBoxed "int"     = "Integer"
javaBoxed "long"    = "Long"
javaBoxed "short"   = "Short"
javaBoxed other     = other

javaParam :: Param -> T.Text
javaParam param = javaType (paramType param) <> " " <> T.pack (paramName param)


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

quoteString :: Char -> T.Text
quoteString '"' = "\\\""
quoteString c = quote c


exprToString :: Expr -> LC T.Text
exprToString (IntLiteral i)      = return $ T.pack $ show i
exprToString (DoubleLiteral d)   = return $ T.pack $ show d
exprToString (BoolLiteral True)  = return "true"
exprToString (BoolLiteral False) = return "false"
exprToString (CharLiteral c)     = return $ "'" <> quoteChar c <> "'"
exprToString (StringLiteral s)   = return $
    "\"" <> T.concat (map quoteString s) <> "\""

exprToString (StringConcat exprs) =
    T.intercalate " + " <$> mapM exprToString exprs

exprToString (ArrayLiteral exprs) = do
    inner <- T.intercalate ", " <$> mapM exprToString exprs
    arrayType <- inferType (ArrayLiteral exprs)
    case arrayType of
        TArray elemType ->
            return $ "new " <> javaType elemType <> "[] { " <> inner <> " }"
        t -> do
            scope <- getScope
            throwError $ LocaleTypeError
                           { lceExpectedType = TArray TAny
                           , lceGotType = t
                           , lceScope = scope
                           }

exprToString (Conditional condition ifTrue ifFalse) = do
    condition' <- exprToString condition
    ifTrue'    <- exprToString ifTrue
    ifFalse'   <- exprToString ifFalse
    return $ "(" <> condition' <> " ? " <> ifTrue' <> " : " <> ifFalse' <> ")"

exprToString (Funcall (ParamName name) args) =
    return $ T.pack name

exprToString (Funcall fpath@(AbsolutePath path) []) =
    return $ T.intercalate "." $ map (call getter . T.pack) path

exprToString (Funcall fpath@(AbsolutePath path) args) =
    T.concat <$> liftM2 (:) path' argList
  where
    path' :: LC T.Text
    path' = do
        isBuiltin <- (`Map.member` builtins) <$> signature
        if isBuiltin
          then (builtins Map.!) <$> signature
          else return $ fmtPath path

    argList :: LC [T.Text]
    argList = sequence [return "(", args', return ")"]

    args' :: LC T.Text
    args' = T.intercalate ", " <$> mapM exprToString args

    signature :: LC TranslationSignature
    signature = do
        paramTypes <- mapM inferType args

        sig <- findGlobalSignature (AbsVarPath path) paramTypes

        let notFound = LocaleSignatureNotFoundError <$> getScope
                                                    <*> pure fpath
                                                    <*> pure paramTypes
        maybe (throwError =<< notFound) return sig

    fmtPath :: [String] -> T.Text
    fmtPath p = T.intercalate "." . mapGetterCall . map T.pack $ p
      where
        mapGetterCall (p0:p1:ps) = call getter p0 : mapGetterCall (p1:ps)
        mapGetterCall l = l


localeOutput :: JavaTarget -> [Locale] -> LC T.Text
localeOutput target locales = liftM T.unlines $ sequence $
    [return preamble] ++

    exportInterface (head locales) : map exportImplementation locales ++

    [return postscript]
  where
    preamble = "package " <> package <> ";\n"
            <> "\n"
            <> "public abstract class " <> iface <> " {\n"
      where
        package = T.pack $ javaPackage target
        iface = T.pack $ javaInterfaceName target

    exportInterface = fmap T.concat . mapM (interface 1) . lcData
    exportImplementation = implementation 1

    postscript = "}"

    -- Interface
    interface :: Int -> TranslationData -> LC T.Text
    interface lvl td@Translation{ tdKey=key, tdParams=params } =
        inInnerScope td $
          liftM T.concat $ sequence $
            case params of
              [] -> [ return $ ind <> "// " <> T.pack key <> "\n"
                    , apply interfaceGetter
                    , apply interfaceConstructor
                    , apply interfaceLazyValue
                    , apply interfaceValue
                    , return "\n"
                    ]

              _  -> [ apply interfaceFunction
                    , return "\n"
                    ]
      where
        apply f = do
            scope <- getScope
            Just sig <- findGlobalSignature (scopePath scope) (map paramType params)
            let returnType = sigReturn sig
            return $ f target lvl (javaType returnType) (T.pack key) params

        ind = javaIndent target lvl

    interface lvl td@NestedData { tdSubGroupName=name, tdNestedData=nd } =
        inInnerScope td $
          T.concat <$> sequence
            [  return $ ind <> "// " <> T.pack name <> "\n"
            , innerInterface
            , apply interfaceGetter
            , apply interfaceConstructor
            , apply interfaceLazyValue
            , apply interfaceValue
            , return "\n"
            ]
      where
        apply f = return $ f target lvl iface (T.pack name) []

        innerInterface :: LC T.Text
        innerInterface =
            liftM T.concat $
              return [ ind <> "public static abstract class " <> iface <> " {\n" ] <->

              mapM (interface $ lvl+1) nd <->

              return [ ind <> "}\n" ]

        (<->) = liftM2 (<>)

        iface = T.pack name
        ind = javaIndent target lvl


    -- Implementation
    implementation :: Int -> Locale -> LC T.Text
    implementation lvl locale =
        T.concat <$> sequence
          [ return $ ind <> signature <> " = " <> "new " <> iface <> "() {\n"

          , dataImplementation (lvl+1) (lcData locale)

          , return $ ind <> "};\n"
          ]
      where
        signature = "public static final " <> iface <> " " <> name

        iface = T.pack $ javaInterfaceName target
        name = T.pack $ lcName locale
        ind = javaIndent target lvl


    dataImplementation :: Int -> [TranslationData] -> LC T.Text
    dataImplementation lvl tds =
        T.concat <$> mapM (dataImplementation' lvl) tds
      where
        isNested NestedData {} = True
        isNested _ = False


    -- single
    dataImplementation' :: Int -> TranslationData -> LC T.Text
    dataImplementation' lvl td@Translation { tdKey=key, tdParams=params } =
        inInnerScope td $
          case params of
            [] -> apply implementConstructor
            _  -> apply implementFunction
      where
        apply f = do
            returnTypeStr <- javaType <$> returnType
            f target lvl returnTypeStr (T.pack key) params fexpr

        returnType = do
            scope <- getScope

            let (AbsVarPath path) = scopePath scope
                notFound = LocaleSymbolNotFoundError scope (AbsolutePath path)

            sigs <- findGlobalSignatures (AbsVarPath path)
            case sigs of
                []    -> throwError notFound
                (s:_) -> return $ sigReturn s

        fexpr _ = exprToString (tdImpl td)


    -- nested
    dataImplementation' lvl td@NestedData { tdSubGroupName=name, tdNestedData=nd } =
        inInnerScope td $ apply implementConstructor
      where
        apply f = f target lvl iface (T.pack name) [] fexpr

        fexpr lvl2 = do
          T.concat <$> sequence
            [ return $ "new " <> iface <> "() {\n"

            , dataImplementation (lvl2+1) nd

            , return $ javaIndent target lvl2  <> "}"
            ]

        iface = T.pack name


interfaceGetter,
  interfaceConstructor,
  interfaceLazyValue,
  interfaceValue,
  interfaceFunction
    :: JavaTarget -> Int -> T.Text -> T.Text -> [Param] -> T.Text


interfaceGetter target lvl returnType name _ =
    ind 0 <> "protected final " <> returnType <> " " <> getter name <> "() {\n" <>
    ind 1 <>   "return " <> lazy name <> " != null \n"                          <>
    ind 2 <>     "? "  <> lazy name                                   <>   "\n" <>
    ind 2 <>     ": (" <> lazy name <> " = " <> call constructor name <> ");\n" <>
    ind 0 <> "}\n"
  where
    ind = javaIndent target . (lvl+)


interfaceConstructor target lvl returnType name _ =
    ind <> "protected abstract " <> returnType <> " " <> constructor name <> "();\n"
  where
    ind = javaIndent target lvl


interfaceLazyValue target lvl returnType name _ =
    ind <> "private " <> javaBoxed returnType <> " " <> lazy name <> ";\n"
  where
    ind = javaIndent target lvl


interfaceValue target lvl returnType name _ =
    ind <> "public final " <> returnType <> " " <> name <> " = "
                                                <> call getter name <> ";\n"
  where
    ind = javaIndent target lvl


interfaceFunction target lvl returnType name params =
    ind <> "public abstract " <> returnType <> " " <> name <> paramList <> ";\n"
  where
    paramList = "("
             <> T.intercalate ", " (map javaParam params)
             <> ")"

    ind = javaIndent target lvl



implementConstructor,
  implementFunction
    :: JavaTarget -> Int -> T.Text -> T.Text -> [Param] -> (Int -> LC T.Text)
    -> LC T.Text

implementConstructor target lvl returnType name _ fexpr =
    T.concat <$> sequence
      [ return $ ind  <> "@Override\n"
      , return $ ind  <> "protected final " <> signature <> " {\n"
      , return $ ind2 <> "return ", fexpr (lvl+1) , return ";\n"
      , return $ ind  <> "}\n"
      ]
  where
    signature = returnType <> " " <> call constructor name

    ind = javaIndent target lvl
    ind2 = javaIndent target (lvl+1)


implementFunction target lvl returnType name params fexpr =
    T.concat <$> sequence
      [ return $ ind  <> "@Override\n"
      , return $ ind  <> "public final " <> signature <> " {\n"
      , return $ ind2 <> "return ", fexpr (lvl+1) , return ";\n"
      , return $ ind  <> "}\n"
      ]
  where
    signature = returnType <> " " <> name <> paramList
    paramList = "("
             <> T.intercalate ", " (map javaParam params)
             <> ")"

    ind = javaIndent target lvl
    ind2 = javaIndent target (lvl+1)



instance Target JavaTarget where
    setEnv target = addAllEnv (Map.keys builtins)

    output target locales = sequence [(,) filePath <$> javaOutput]
      where
        javaOutput = localeOutput target locales
        filePath = javaDirname target </> packagePath </> fileName
        packagePath = foldl1' (</>) $ splitOn "." (javaPackage target)
        fileName = javaInterfaceName target ++ ".java"
