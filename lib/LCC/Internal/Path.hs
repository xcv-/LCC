module LCC.Internal.Path

import Control.Comonad
import Control.Lens.Lens
import Control.Lens.Prism


type Node = String


data RelativePath = RAbsolutePath [Node]
                  | RRelativePath [Node]
    deriving (Eq, Ord)

newtype AbsolutePath = AbsolutePath [Node]
    deriving (Eq, Ord, Monoid, Functor, Applicative, Monad, Comonad)


data RelativeVarPath = RVAbsoluteVarPath [Node]
                     | RVRelativeVarPath [Node]
                     | RVParamName       Node
    deriving (Eq, Ord)

data AbsoluteVarPath = VAbsolutePath [Node]
                     | VParamName    Node
    deriving (Eq, Ord)




-- Overloaded constructors

class FromAbsolute path where
    mkAbsolute :: [Node] -> path

class FromParamName path where
    mkParamName :: Node -> path

class FromRelative path where
    mkRelative :: [Node] -> path



instance FromRelative RelativeVarPath where
    mkRelative = RRelativeVarPath


instance FromParamName RelativeVarPath  where
    mkParamName = RParamName

instance FromParamName VarPath where
    mkParamName = ParamName


instance FromAbsolute RelativeVarPath where
    mkAbsolute = RAbsoluteVarPath

instance FromAbsolute VarPath where
    mkAbsolute = AbsoluteVarPath

instance FromAbsolute AbsolutePath where
    mkAbsolute = AbsolutePath




-- Instances

defaultShow = show . mconcat . intersperse "."

instance Show RelativePath where
    show (RRelativePath path) = defaultShow path
    show (RAbsolutePath path) = defaultShow path

instance Show AbsolutePath where
    show (AbsolutePath path) = defaultShow path


instance Show RelativeVarPath where
    show (RVRelativePath path) = defaultShow path
    show (RVAbsolutePath path) = defaultShow path
    show (RVParamName path)    = "@" <> name

instance Show AbsoluteVarPath where
    show (VAbsolutePath path) = defaultShow path
    show (VParamName name)    = "@" <> name


-- Lens

absolute :: Lens' AbsolutePath [String]
absolute = lens extract ($>)


-- Prisms

class RelativePrism path where
    _relative :: SimplePrism path [String]

class ParamNamePrism path where
    _paramName :: SimplePrism path String

class AbsolutePrism path where
    _absolute :: SimplePrism path [String]




instance AbsolutePrism RelativePath where
    _relative = prism RRelativePath $ \case of
        RRelativePath x -> Right x
        x               -> Left x

instance AbsolutePrism RelativeVarPath where
    _relative = prism RVAbsoluteVarPath $ \case of
        RVRelativeVarPath x -> Right x
        x                   -> Left x


instance ParamNamePrism RelativeVarPath where
    _paramName = prism RVParamName $ \case of
        RVParamName x -> Right x
        x             -> Left x

instance ParamNamePrism AbsoluteVarPath where
    _paramName = prism VParamName $ \case of
        VParamName x -> Right x
        x            -> Left x


instance AbsolutePrism RelativePath where
    _absolute = prism RAbsolutePath $ \case of
        RAbsolutePath x -> Right x
        x               -> Left x

instance AbsolutePrism AbsolutePath where
    _absolute = prism AbsolutePath $ Right . extract


instance AbsolutePrism RelativeVarPath where
    _absolute = prism RVAbsoluteVarPath $ \case of
        RVAbsoluteVarPath x -> Right x
        x                   -> Left x

instance AbsolutePrism AbsoluteVarPath where
    _absolute = prism VAbsolutePath $ \case of
        VAbsolutePath x -> Right x
        x               -> Left x
