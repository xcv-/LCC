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


findGlobalSignatures :: MonadState LocaleState m
                     => AbsVarPath
                     -> m [TranslationSignature]
findGlobalSignatures path =
    filter (\sig -> sigPath sig == path) <$> signatures
  where
    signatures :: MonadState LocaleState m => m [TranslationSignature]
    signatures = Set.toList . envSignatures <$> getEnv

findGlobalSignature :: MonadState LocaleState m
                    => AbsVarPath
                    -> [Type]
                    -> m (Maybe TranslationSignature)
findGlobalSignature path paramTypes =
    find (matchParams paramTypes) <$> findGlobalSignatures path


findSignatures :: MonadState LocaleState m
               => VarPath
               -> m [SymbolSignature]
findSignatures (AbsolutePath path) =
    map toVarPath <$> findGlobalSignatures (AbsVarPath path)
  where
    toVarPath sig@Signature { sigPath=(AbsVarPath path) } =
        sig { sigPath=AbsolutePath path }

findSignatures (ParamName name) = do
    scope <- getScope
    case scope of
        SubGroupScope _ -> return []
        TranslationScope _ params  ->
            return . map paramSignature . filterParams name $ params


findSignature :: MonadState LocaleState m
              => VarPath
              -> [Type]
              -> m (Maybe SymbolSignature)
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


getScope :: MonadState LocaleState m => Scope
getScope = lcsScope <$> get

getScopePath :: MonadState LocaleState m => m AbsVarPath
getScopePath = scopePath <$> getScope

modifyScope :: MonadState LocaleState m => (Scope -> Scope) -> m ()
modifyScope f = modify $ \ls -> ls { lcsScope = f (lcsScope ls) }

inScope :: MonadState LocaleState m => Scope -> m a -> m a
inScope scope m = do
    st <- get
    modifyScope (const scope)
    x <- m
    put st
    return x

inInnerScope :: (MonadState LocaleState m, MonadError LocaleError m)
             => GenericTranslationData path
             -> m a
             -> m a
inInnerScope Translation { tdKey=key, tdParams=params } m = do
    scope <- getScope
    case appendedToScopePath key scope of
        Just newPath -> inScope (TranslationScope newPath params) m
        Nothing      ->
            let (AbsVarPath path) = scopePath scope
            in throwError $ LocalePathError scope
                                            ("Cannot navigate to " ++ key)
                                            (AbsolutePath path)

inInnerScope  NestedData { tdSubGroupName=name } m = do
    scope <- getScope
    case appendedToScopePath name scope of
        Just newPath -> inScope (SubGroupScope newPath) m
        Nothing      ->
            let (AbsVarPath path) = scopePath scope
            in throwError $ LocalePathError scope
                                            ("Cannot navigate to " ++ name)
                                            (AbsolutePath path)


-- Environment

emptyEnv :: Env
emptyEnv = Env Set.empty


getEnv :: MonadState LocaleState m => m Env
getEnv = lcsEnv <$> get

modifyEnv :: MonadState LocaleState m=> (Env -> Env) -> m ()
modifyEnv f = modify $ \ls -> ls { lcsEnv = f (lcsEnv ls) }

addEnv :: MonadState LocaleState m => TranslationSignature -> m ()
addEnv sig = modifyEnv $ Env . Set.insert sig . envSignatures

addAllEnv :: MonadState LocaleState m => [TranslationSignature] -> m ()
addAllEnv = mapM_ addEnv
