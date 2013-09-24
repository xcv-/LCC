module LCC.State
  ( LocaleState

  , emptyState

  , topLevelScope
  , scopePath
  , getScope
  , getScopePath
  , inScope
  , inInnerScope

  , emptyEnv
  , getEnv
  , modifyEnv
  , addEnv
  , addAllEnv

  , filterParams

  , findGlobalSignatures
  , findGlobalSignature

  , findSignatures
  , findSignature
  ) where


import Data.List
import Data.Functor ((<$>))

import Control.Monad.Error
import Control.Monad.State

import qualified Data.Set as Set

import LCC.Internal.Types


emptyState :: LocaleState
emptyState = LocaleState { lcsScope = topLevelScope
                         , lcsEnv = emptyEnv
                         }


-- Signatures

paramSignature :: Param -> SymbolSignature
paramSignature param = Signature
    { sigPath = ParamName (paramName param)
    , sigParams = []
    , sigReturn = paramType param
    }

filterParams :: String -> [Param] -> [Param]
filterParams name = filter (\p -> paramName p == name)

matchParams :: [Type] -> GenericSignature p -> Bool
matchParams types signature = map paramType (sigParams signature) == types


findGlobalSignatures :: AbsVarPath -> LC [TranslationSignature]
findGlobalSignatures path =
    filter (\sig -> sigPath sig == path) <$> signatures
  where
    signatures :: LC [TranslationSignature]
    signatures = Set.toList . envSignatures <$> getEnv

findGlobalSignature :: AbsVarPath -> [Type] -> LC (Maybe TranslationSignature)
findGlobalSignature path paramTypes =
    find (matchParams paramTypes) <$> findGlobalSignatures path


findSignatures :: VarPath -> LC [SymbolSignature]
findSignatures (AbsolutePath path) =
    map toVarPath <$> findGlobalSignatures (AbsVarPath path)
  where
    toVarPath sig@Signature { sigPath=(AbsVarPath path) } =
        sig { sigPath=(AbsolutePath path) }

findSignatures (ParamName name) = do
    scope <- getScope
    case scope of
        SubGroupScope _ -> return []
        TranslationScope _ params  ->
            return . map paramSignature . filterParams name $ params

findSignature :: VarPath -> [Type] -> LC (Maybe SymbolSignature)
findSignature path paramTypes =
    find (matchParams paramTypes) <$> findSignatures path


-- Scope

topLevelScope :: Scope
topLevelScope = SubGroupScope (AbsVarPath [])

scopePath :: Scope -> AbsVarPath
scopePath (TranslationScope path _) = path
scopePath (SubGroupScope path)      = path

appendedToScopePath :: String -> Scope -> Maybe AbsVarPath
appendedToScopePath name (TranslationScope _ _) = Nothing
appendedToScopePath name (SubGroupScope (AbsVarPath path)) =
    Just $ AbsVarPath (path ++ [name])


getScope :: LC Scope
getScope = lcsScope <$> get

getScopePath :: LC AbsVarPath
getScopePath = scopePath <$> getScope

modifyScope :: (Scope -> Scope) -> LC ()
modifyScope f = modify $ \ls -> ls { lcsScope = f (lcsScope ls) }

inScope :: Scope -> LC a -> LC a
inScope scope m = do
    st <- get
    modifyScope (const scope)
    x <- m
    put st
    return x

inInnerScope :: GenericTranslationData path -> LC a -> LC a
inInnerScope Translation { tdKey=key, tdParams=params } m = do
    scope <- getScope
    case appendedToScopePath key scope of
        Just newPath -> inScope (TranslationScope newPath params) m
        Nothing      ->
            let (AbsVarPath path) = scopePath scope
            in throwError $ LocalePathError ("Cannot navigate to " ++ key)
                                         (AbsolutePath path)

inInnerScope  NestedData { tdSubGroupName=name } m = do
    scope <- getScope
    case appendedToScopePath name scope of
        Just newPath -> inScope (SubGroupScope newPath) m
        Nothing      ->
            let (AbsVarPath path) = scopePath scope
            in throwError $ LocalePathError ("Cannot navigate to " ++ name)
                                         (AbsolutePath path)


-- Environment

emptyEnv :: Env
emptyEnv = Env Set.empty


getEnv :: LC Env
getEnv = lcsEnv <$> get

modifyEnv :: (Env -> Env) -> LC ()
modifyEnv f = modify $ \ls -> ls { lcsEnv = f (lcsEnv ls) }

addEnv :: TranslationSignature -> LC ()
addEnv sig = modifyEnv $ Env . Set.insert sig . envSignatures

addAllEnv :: [TranslationSignature] -> LC ()
addAllEnv = mapM_ addEnv
