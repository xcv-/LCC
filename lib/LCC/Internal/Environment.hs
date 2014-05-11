module LCC.Internal.Environment where


import Types

newtype Env = Env { _envSignatures :: Set.Set TranslationSignature }
    deriving (Eq, Ord, Show)


paramSignature :: Param -> VarSignature
paramSignature param = Signature
    { _sigPath = ParamName (paramName param)
    , _sigParams = []
    , _sigReturn = paramType param
    }

filterParams :: String -> [Param] -> [Param]
filterParams name =
    filter (\p -> paramName p == name)

matchParams :: [Type] -> Signature path -> Bool
matchParams types signature =
    map paramType (sigParams signature) == types


findGlobalSignatures :: MonadState Env m => AbsolutePath -> m [GlobalSignature]
findGlobalSignatures path =
    filter (\sig -> sig^.sigPath == path) <$> signatures
  where
    signatures :: MonadState Env m => m [GlobalSignature]
    signatures = use $ envSignatures^..traverse


findGlobalSignature :: MonadState Env m
                    => AbsolutePath
                    -> [Type]
                    -> m (Maybe GlobalSignature)
findGlobalSignature path paramTypes =
    find (matchParams paramTypes) <$> findGlobalSignatures path


findSignatures :: (MonadState Env m, Scoped m)
               => VarPath
               -> m [VarSignature]
findSignatures (VAbsolutePath path) =
    map toVarPath <$> findGlobalSignatures (AbsolutePath path)
  where
    toVarPath sig@Signature {} = sig^.sigPath %~ absolute.extract

findSignatures (VParamName name) = do
    scope <- get
    case scope of
        SubGroupScope _ -> return []
        TranslationScope _ params  ->
            return . map paramSignature . filterParams name $ params


findSignature :: MonadState Env m => VarPath -> [Type] -> m (Maybe VarSignature)
findSignature path paramTypes =
    find (matchParams paramTypes) <$> findSignatures path
