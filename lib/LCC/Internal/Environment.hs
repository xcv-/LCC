{-# LANGUAGE ConstraintKinds #-}
module LCC.Internal.Environment where

import qualified Data.Map as Map

import LCC.Internal.AST
import LCC.Internal.Path
import LCC.Internal.Signature



newtype Env path ret = Env
    { _envMap :: Map.Map (Signature AbsolutePath ret) (Expr path)
    }
  deriving (Eq, Ord, Show)


type EnvM path ret = MonadState (Env path ret)


paramSignature :: Param -> Signature AbsoluteVarPath Type
paramSignature param = Signature
    { _sigPath = ParamName (param^.paramName)
    , _sigParams = []
    , _sigReturn = param^.paramType
    }


filterParams :: String -> [Param] -> [Param]
filterParams name =
    filter (\p -> p^.paramName == name)

matchParams :: [Type] -> Signature path ret -> Bool
matchParams types signature =
    signature^..sigParams.traverse.paramType == types



findGlobalSignatures :: EnvM AbsolutePath ret m
                     => AbsolutePath
                     -> m [Signature AbsolutePath ret]
findGlobalSignatures path =
    gets $ toListOf $
      envMap.to Map.keys.folded.filtered (\sig -> sig^.sigPath == path)


findGlobalSignature :: EnvM AbsolutePath ret m
                    => AbsolutePath
                    -> [Type]
                    -> m (Maybe (Signature AbsolutePath ret))
findGlobalSignature path paramTypes =
    find (matchParams paramTypes) <$> findGlobalSignatures path



findSignatures :: (EnvM AbsoluteVarPath ret m, ScopedAST AbsoluteVarPath ret m)
               => AbsoluteVarPath
               -> m [Signature AbsoluteVarPath ret]
findSignatures path =
    case path of
      VAbsolutePath p ->
        findGlobalSignatures (mkAbsolute p)
          <&> map (sigPath %~ mkAbsolute . view absolute)

      VParamName name ->
        use (toListOf $ scopeData.trSignature.sigParams)
          <&> map paramSignature . filterParams name


findSignature :: EnvM AbsoluteVarPath ret m
              => AbsoluteVarPath
              -> [Type]
              -> m (Maybe (Signature AbsoluteVarPath ret))
findSignature path paramTypes =
    find (matchParams paramTypes) <$> findSignatures path



findGlobalImpl :: EnvM path ret m
               => Signature AbsolutePath ret
               -> m (Maybe (Expr path))
findGlobalImpl sig =
    use $ envMap.at sig
