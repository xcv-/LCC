module LCC.Internal.Scope where

import Control.Monad.Error
import Text.Printf (printf)

import LCC.Internal.Error
import LCC.Internal.Path


type Scoped = MonadState Scope

data Scope = LeafScope { _scopePath   :: AbsolutePath
                       , _scopeParams :: [Param]
           | SubtreeScope { _scopePath :: AbsolutePath
                          }
    deriving Eq

instance Show Scope where
    show (LeafScope path params) =
        printf "%s(%s)" (show path) (intercalate ", " $ map show params)

    show (SubtreeScope path) =
        printf "%s" (show path)


topLevelScope :: Scope
topLevelScope = SubtreeScope mempty


extendScope :: Scope -> PathNode -> Maybe Scope
extendScope LeafScope {} _ = Nothing
extendScope scope@SubtreeScope {} node =
    Just $ node & scopePath.absolute %~ (<> [node])


inScope :: MonadState Scope m => Scope -> m a -> m a
inScope scope m = do
    oldScope <- get
    put scope
    x <- m
    put oldScope
    return x

inInnerScope, (/>) :: (Scoped m, MonadError LocaleError m)
                   => TranslationTree path
                   -> m a
                   -> m a
inInnerScope leaf@Leaf {} = do
    scope <- get

    case extendScope scope (leaf^.name) of
        Just newPath ->
            inScope (LeafScope newPath (leaf^.params) m

        Nothing ->
            throwError $ LocalePathError scope
                                         ("Cannot navigate to " ++ key)
                                         (scope^.scopePath.absolute)

inInnerScope subtree@Subtree {} =
    scope <- get

    case extendScope scope (subtree^.name) of
        Just newPath ->
            inScope (SubtreeScope newPath) m

        Nothing ->
            throwError $ LocalePathError scope
                                         ("Cannot navigate to " ++ name)
                                         (scope^.scopePath.absolute)


(/>) = inInnerScope


makeLenses ''Scope
