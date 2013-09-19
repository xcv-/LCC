module LCC.Analyzer
  ( checkInterfaces
  , compile
  , inferType
  ) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad.Error
import Control.Monad.State

import Data.Either
import Data.List
import Data.Traversable (for)
import qualified Data.Text as T
import qualified Data.Set as Set

import Text.Parsec

import LCC.Internal.Types
import qualified LCC.Lexer as Lex

import LCC.Parser
import LCC.State
import LCC.DependencyGraph


checkInterfaces :: [(Locale, LocaleState)]
                -> Either LocaleError [(Locale, LocaleState)]
checkInterfaces [] = Right []
checkInterfaces states =
    zipWithM applyCheck states (init $ cycle states)
  where
    applyCheck (lc1, st1) (lc2, st2) =
        case checkInterface st1 st2 of
            ([],[]) -> Right (lc1, st1)
            ([],missing) -> Left $ LocaleInterfaceError lc2 missing
            (missing, _) -> Left $ LocaleInterfaceError lc1 missing

checkInterface :: LocaleState -> LocaleState
               -> ([TranslationSignature], [TranslationSignature])
checkInterface st1 st2 =
    let sigs1 = Set.toList $ envSignatures $ lcsEnv st1
        sigs2 = Set.toList $ envSignatures $ lcsEnv st2
    in (sigs2 \\ sigs1, sigs1 \\ sigs2)


compile :: RawLocale -> LC Locale
compile rawLocale = do
    lc <- makeAbsolute rawLocale

    verifyNameLookup lc
    populateEnv lc
    typeCheck lc

    return lc

collectPaths :: Locale -> Set.Set AbsVarPath
collectPaths lc = Set.fromList . concatMap (collectPaths' []) $ lcData lc
  where
    collectPaths' :: [String] -> TranslationData -> [AbsVarPath]
    collectPaths' path Translation { tdKey=key } =
        [AbsVarPath $ path ++ [key]]
    collectPaths' path NestedData { tdNestedData=[] } =
        [AbsVarPath []]
    collectPaths' path NestedData { tdSubGroupName=name, tdNestedData=nested } =
        concatMap (collectPaths' $ path ++ [name]) nested


verifyNameLookup :: Locale -> LC ()
verifyNameLookup lc@Locale { lcData=ld } = do
    builtins <- Set.map sigPath . envSignatures <$> getEnv
    let known = Set.union (collectPaths lc) builtins
    mapM_ (verifyNameLookup' known) ld
  where
    verifyNameLookup' :: Set.Set AbsVarPath -> TranslationData -> LC ()
    verifyNameLookup' known td@Translation { tdImpl=impl } =
        inInnerScope td $ verifyExprNameLookup known impl

    verifyNameLookup' known td@NestedData { tdNestedData=nested } =
        inInnerScope td $ mapM_ (verifyNameLookup' known) nested

    verifyExprNameLookup :: Set.Set AbsVarPath -> Expr -> LC ()
    verifyExprNameLookup known (ArrayLiteral exprs) =
        mapM_ (verifyExprNameLookup known) exprs

    verifyExprNameLookup known (StringConcat exprs) =
        mapM_ (verifyExprNameLookup known) exprs

    verifyExprNameLookup known (Funcall path@(AbsolutePath path') args)
      | AbsVarPath path' `Set.member` known =
          mapM_ (verifyExprNameLookup known) args
      | otherwise =
          throwError =<< LocaleSymbolNotFoundError <$> getScope <*> pure path

    verifyExprNameLookup _ (Funcall path@(ParamName name) args) = do
        scope <- getScope
        case scope of
            (TranslationScope _ params)
              | null $ filterParams name params ->
                  throwError $ LocaleSymbolNotFoundError scope path
              | otherwise ->
                  return ()
            (SubGroupScope _) -> throwError $ LocaleError $
                "Evaluating expression in SubGroupScope: " ++ show scope

    verifyExprNameLookup _ _ = return ()


populateEnv :: Locale -> LC ()
populateEnv lc = do
    builtins <- Set.map sigPath . envSignatures <$> getEnv

    let adaptNode (path, params, expr) = (path, (expr, params))
        nodes = map adaptNode flattened
        listKeys (expr, params) = typeInferenceDependencies builtins expr
        (graph, fromVertex, toVertex) = dependencyGraph nodes listKeys

    forM_ (sortGraph graph) $ \vert ->
        let ((expr, params), path, _) = fromVertex vert
            returnType = inScope (TranslationScope path params) $ inferType expr
        in addEnv =<< Signature path params <$> returnType
  where
    flattened :: [(AbsVarPath, [Param], Expr)]
    flattened = concatMap flatten $ lcData lc

    flatten :: TranslationData -> [(AbsVarPath, [Param], Expr)]
    flatten td@NestedData { tdSubGroupName=name, tdNestedData=nested } =
        concatMap (map (append name) . flatten) nested
      where
        append name (AbsVarPath path, params, expr) =
            (AbsVarPath $ name : path, params, expr)

    flatten Translation { tdKey=key, tdParams=params, tdImpl=expr } =
        [(AbsVarPath [key], params, expr)]


makeAbsolute :: RawLocale -> LC Locale
makeAbsolute lc@Locale { lcData=ld } = do
    transformed <- mapM makeAbsolutePaths ld
    return $ lc { lcData=transformed }
  where
    makeAbsolutePaths :: RawTranslationData -> LC TranslationData
    makeAbsolutePaths td@Translation { tdImpl=impl } = do
        impl' <- inInnerScope td $ makeAbsoluteExpr impl
        return $ td { tdImpl = impl' }

    makeAbsolutePaths td@NestedData { tdNestedData=nested } = do
        nested' <- inInnerScope td $ mapM makeAbsolutePaths nested
        return $ td { tdNestedData=nested' }

    makeAbsoluteExpr :: RawExpr -> LC Expr
    makeAbsoluteExpr (StringConcat exprs) =
        StringConcat <$> mapM makeAbsoluteExpr exprs

    makeAbsoluteExpr (ArrayLiteral exprs) =
        ArrayLiteral <$> mapM makeAbsoluteExpr exprs

    makeAbsoluteExpr (Funcall path args) =
        Funcall <$> makeAbsolutePath path <*> mapM makeAbsoluteExpr args

    makeAbsoluteExpr (IntLiteral x)    = return $ IntLiteral x
    makeAbsoluteExpr (DoubleLiteral x) = return $ DoubleLiteral x
    makeAbsoluteExpr (BoolLiteral x)   = return $ BoolLiteral x
    makeAbsoluteExpr (CharLiteral x)   = return $ CharLiteral x
    makeAbsoluteExpr (StringLiteral x) = return $ StringLiteral x

makeAbsolutePath :: RawVarPath -> LC VarPath
makeAbsolutePath (RawAbsolutePath path)        = return $ AbsolutePath path
makeAbsolutePath (RawParamName name)           = return $ ParamName name
makeAbsolutePath initial@(RawRelativePath rel) = do
    (AbsVarPath scopePath) <- getScopePath
    AbsolutePath <$> makeAbsolutePath' rel (reverse scopePath)
  where
    makeAbsolutePath' :: [String] -> [String] -> LC [String]
    makeAbsolutePath' [] scopePath = return $ reverse scopePath

    makeAbsolutePath' ("^":rest) [] =
        throwError =<< LocaleRelativePathError <$> getScope <*> pure initial

    makeAbsolutePath' ("^":rest) (current:acc) = makeAbsolutePath' rest acc

    makeAbsolutePath' (toEnter:rest) acc = makeAbsolutePath' rest (toEnter:acc)


typeCheck :: Locale -> LC ()
typeCheck Locale { lcData = ld } = mapM_ typeCheck' ld
  where
    typeCheck' :: TranslationData -> LC ()
    typeCheck' td@Translation { tdImpl=impl } = do
        inInnerScope td $ exprType checkTypesEqual impl
        return ()

    typeCheck' td@NestedData { tdNestedData=nested } =
        inInnerScope td $ mapM_ typeCheck' nested

inferType :: Expr -> LC Type
inferType = exprType inferCommonType


exprType :: (Type -> [Expr] -> LC Type) -> Expr -> LC Type
exprType _ (IntLiteral _)    = return TInt
exprType _ (DoubleLiteral _) = return TDouble
exprType _ (BoolLiteral _)   = return TBool

exprType _ (CharLiteral _)   = return TChar
exprType _ (StringLiteral _) = return TString

exprType fold (StringConcat exprs) = do
    commonType <- fold TString exprs
    case commonType of
        TAny    -> return TString
        TString -> return TString
        other   -> throwError . LocaleTypeError TString other =<< getScope

exprType fold (ArrayLiteral exprs) = TArray <$> fold TAny exprs

exprType fold (Funcall path args) = do
    argTypes <- mapM (exprType fold) args
    maybeSig <- findSignature path argTypes
    case maybeSig of
        Just sig -> return $ sigReturn sig
        Nothing  -> throwError =<<
            LocaleSignatureNotFoundError <$> getScope <*> pure path <*> pure argTypes


inferCommonType :: Type -> [Expr] -> LC Type
inferCommonType TAny exprs =
    foldl f TAny <$> mapM (exprType inferCommonType) exprs
  where
    f TAny t = t
    f t    _ = t

inferCommonType expected _ = return expected


checkTypesEqual :: Type -> [Expr] -> LC Type
checkTypesEqual expected [] = return TAny
checkTypesEqual expected exprs = do
    (type0:types) <- mapM (exprType checkTypesEqual) exprs
    foldM checkEqual type0 types
  where
    checkEqual :: Type -> Type -> LC Type
    checkEqual x y
      | x == y    = return x
      | otherwise = getScope >>= throwError . LocaleTypeError x y

