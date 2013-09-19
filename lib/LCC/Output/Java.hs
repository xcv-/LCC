{-# LANGUAGE OverloadedStrings #-}
module LCC.Output.Java
  ( JavaTarget (..)
  ) where

import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.Error
import Control.Applicative

import System.IO
import System.FilePath ((</>))

import Data.List
import Data.List.Split

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

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

builtins :: M.Map TranslationSignature T.Text
builtins = M.fromList $ builtinMap
    [ (["str"], [TChar  ], TString, "Character.toString")
    , (["str"], [TString], TString, ""                  )
    , (["str"], [TInt   ], TString, "Integer.toString"  )
    , (["str"], [TDouble], TString, "Double.toString"   )
    ]
  where
    builtinMap = map $ \(path, paramTypes, ret, replacement) ->
        ( Signature { sigPath = AbsVarPath path
                    , sigParams = map builtinParam paramTypes
                    , sigReturn = ret
                    }
        , replacement
        )



javaIndent :: JavaTarget -> Int -> T.Text
javaIndent target count = T.replicate count unit
  where
    unit = if javaExpandTab target
             then T.replicate (javaTabWidth target) " "
             else "\t"

javaType :: Type -> T.Text
javaType TInt    = "int"
javaType TDouble = "double"
javaType TBool   = "boolean"
javaType TChar   = "char"
javaType TString = "String"
javaType (TArray e) = javaType e <> "[]"

javaParam :: Param -> T.Text
javaParam param = javaType (paramType param) <> " " <> T.pack (paramName param)

getter :: T.Text -> T.Text
getter name = "__get_" <> name


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
exprToString (IntLiteral i)    = return $ T.pack $ show i
exprToString (DoubleLiteral d) = return $ T.pack $ show d
exprToString (CharLiteral c)   = return $ "'" <> quoteChar c <> "'"
exprToString (StringLiteral s) = return $
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

exprToString (Funcall (ParamName name) args) =
    return $ T.pack name

exprToString (Funcall (AbsolutePath path) args) =
    T.concat <$> liftM2 (:) name' argList
  where
    name' :: LC T.Text
    name' = do
        signature' <- signature
        if signature' `M.member` builtins
          then return $ builtins M.! signature'
          else return $ fmtPath path

    argList :: LC [T.Text]
    argList = sequence [return "(", args', return ")"]

    args' :: LC T.Text
    args' = T.intercalate ", " <$> mapM exprToString args

    signature :: LC TranslationSignature
    signature = do
        params <- forM args $ \arg -> builtinParam <$> inferType arg
        return $ Signature (AbsVarPath path) params TString

    fmtPath :: [String] -> T.Text
    fmtPath p = T.intercalate "." . mapGetter . map T.pack $ p
      where
        mapGetter [] = []
        mapGetter [p] = [getter p]
        mapGetter (p:ps) = getter p <> "()" : mapGetter ps


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

    exportInterface = return . T.concat . map (interface 1) . sortData . lcData
    exportImplementation = instantiation 1

    postscript = "}"

    sortData :: [TranslationData] -> [TranslationData]
    sortData = uncurry (++) . partition isNested
      where
        isNested NestedData{} = True
        isNested _ = False


    -- Interface
    interface :: Int -> TranslationData -> T.Text
    interface lvl td@Translation{ tdKey=key, tdParams=params } =
           ind <> apply publicAccessor
        <> ind <> apply privateInterface
        <> "\n"
      where
        apply f = f lvl "String" (T.pack key) params

        ind = javaIndent target lvl


    interface lvl NestedData { tdSubGroupName=name, tdNestedData=nd } =
           innerInterface
        <> ind <> apply publicAccessor
        <> ind <> apply privateInterface
        <> "\n"
      where
        apply f = f lvl iface (T.pack name) []

        innerInterface =
          T.concat $
            [ ind <> "public static abstract class " <> iface <> " {\n" ] ++

            map (interface $ lvl+1) (sortData nd) ++

            [ ind <> "}\n" ]

        iface = T.pack name
        ind = javaIndent target lvl


    -- Implementation
    instantiation :: Int -> Locale -> LC T.Text
    instantiation lvl locale =
        T.concat <$> sequence
          [ return $ ind <> signature <> " = " <> "new " <> iface <> "() {\n"

          , dataInstantiation (lvl+1) (lcData locale)

          , return $ ind <> "};\n"
          ]
      where
        signature = "public static final " <> iface <> " " <> name

        iface = T.pack $ javaInterfaceName target
        name = T.pack $ lcName locale
        ind = javaIndent target lvl


    dataInstantiation :: Int -> [TranslationData] -> LC T.Text
    dataInstantiation lvl tds =
        T.concat <$> mapM (dataInstantiation' lvl) (sortData tds)
      where
        isNested NestedData {} = True
        isNested _ = False


    -- single
    dataInstantiation' :: Int -> TranslationData -> LC T.Text
    dataInstantiation' lvl td@Translation { tdKey=key, tdParams=params } =
        inInnerScope td $ apply implementHelperGetter expr
      where
        apply f e = do
            rt <- javaType <$> returnType
            f lvl rt (T.pack key) params e

        returnType = do
            scope <- getScope
            let (AbsVarPath path) = scopePath scope
            sigs <- findGlobalSignatures (AbsVarPath path)
            case sigs of
                [] ->
                    throwError $ LocaleSymbolNotFoundError scope (AbsolutePath path)
                (s:ss) ->
                    return $ sigReturn s

        expr _ = exprToString (tdImpl td)


    -- nested
    dataInstantiation' lvl td@NestedData { tdSubGroupName=name, tdNestedData=nd } =
        inInnerScope td $
          apply implementHelperGetter >>= ($expr)
      where
        apply f = do
          iface' <- iface
          return $ f lvl iface' (T.pack name) []

        expr lvl2 = do
          iface' <- iface

          T.concat <$> sequence
            [ return $ "new " <> iface' <> "() {\n"

            , dataInstantiation (lvl2+1) nd

            , return $ javaIndent target lvl2  <> "}"
            ]

        iface = do
            (AbsVarPath path) <- getScopePath
            return $ T.intercalate "." $ map T.pack path

    publicAccessor :: Int -> T.Text -> T.Text -> [Param] -> T.Text
    publicAccessor lvl returnType name params =
        "public final " <> returnType <> " " <> name <> definition
      where
        fmtParams = "("
                 <> T.intercalate "," (map javaParam params)
                 <> ")"

        definition = fmtParams <> " {\n"
                  <> ind2 <> "return " <> value <> ";\n"
                  <> ind <> "}\n"

        value = getterCall

        getterCall = getter name <> fmtArgs

        fmtArgs = "("
               <> T.intercalate "," (map (T.pack . paramName) params)
               <> ")"

        ind = javaIndent target lvl
        ind2 = javaIndent target (lvl+1)

    privateInterface :: Int -> T.Text -> T.Text -> [Param] -> T.Text
    privateInterface lvl returnType name params =
        helperGetterSignature lvl returnType name params <> ";\n"


    helperGetterSignature :: Int -> T.Text -> T.Text -> [Param] -> T.Text
    helperGetterSignature lvl returnType name params =
        "protected " <> returnType <> " " <> getter name <> fmtParams
      where
        fmtParams = "("
                 <> T.intercalate "," (map javaParam params)
                 <> ")"

    implementHelperGetter :: Int -> T.Text -> T.Text -> [Param]
                          -> (Int -> LC T.Text)
                          -> LC T.Text
    implementHelperGetter lvl returnType name params expr =
        T.concat <$> sequence
          [ return $ ind <> "@Override\n"
          , return $ ind <> signature <> " {\n"
          , return $ ind2 <> "return ", expr (lvl+1) , return ";\n"
          , return $ ind <> "}\n"
          ]
      where
        signature = helperGetterSignature lvl returnType name params
        ind = javaIndent target lvl
        ind2 = javaIndent target (lvl+1)


instance Target JavaTarget where
    setEnv target = addAllEnv (M.keys builtins)

    output target locales = sequence [(,) filePath <$> javaOutput]
      where
        javaOutput = localeOutput target locales
        filePath = javaDirname target </> packagePath </> fileName
        packagePath = foldl1' (</>) $ splitOn "." (javaPackage target)
        fileName = javaInterfaceName target ++ ".java"
