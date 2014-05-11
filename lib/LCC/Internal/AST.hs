

class TaggedTreeData where
    type Tag
    type Data


data TaggedTree a
    = Subtree { _name  :: Tag a
              , _nodes :: [TaggedTree a]
              }
    | Leaf { _name    :: Tag a
           , _content :: Data a
           }


type ASTData path = (AbsolutePath, Translation path)

instance TaggedTreeData (ASTData path) where
    type Tag = String
    type Data = Translation path


data AST path = TaggedTree ASTData
    deriving (Show)

instance (TaggedTreeData a, Monoid (Data TaggedTreeData)) =>
      Foldable TaggedTree where

    foldMap f Leaf {..} = f (pure _name, _nodes)
    foldMap f s@Subtree {} = foldMap' [] f s
      where
        foldMap' path



type TranslationData = GenericTranslationData VarPath
type RawTranslationData = GenericTranslationData RawVarPath

type LocaleImplEnv = Map.Map (VarPath, [Type]) ([Param], Expr)
type LocaleImplEnvNode = (AbsVarPath, [Param], Expr)

data LocaleImpl = LocaleImpl { _lciName :: String
                             , _lciEnv  :: LocaleImplEnv
                             }


data GenericTranslationData path =
        Translation { tdKey    :: String
                    , tdParams :: [Param]
                    , tdImpl   :: GenericExpr path
                    }
      | NestedData { tdSubGroupName :: String
                   , tdNestedData :: [GenericTranslationData path]
                   }
    deriving (Show)

