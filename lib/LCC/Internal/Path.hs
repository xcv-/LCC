module LCC.Internal.Path

import Control.Comonad
import Control.Lens.Lens
import Control.Lens.Prism

import qualified Data.Seq as Seq


type PathNode = String
type Seq a = Seq.Seq a


data RelativePath = RAbsolutePath (Seq PathNode)
                  | RRelativePath (Seq PathNode)
    deriving (Eq, Ord)

newtype AbsolutePath = AbsolutePath (Seq PathNode)
    deriving (Eq, Ord, Monoid,
              Functor, Applicative, Monad, Comonad)


data RelativeVarPath = RVAbsoluteVarPath (Seq PathNode)
                     | RVRelativeVarPath (Seq PathNode)
                     | RVParamName       PathNode
    deriving (Eq, Ord)

data AbsoluteVarPath = VAbsolutePath (Seq PathNode)
                     | VParamName    PathNode
    deriving (Eq, Ord)



(|>~) :: Snoc sn sn a a => Setting (->) s t sn sn -> a -> s -> t
lens |>~ a = lens %~ (|> a)



-- Overloaded constructors

class FromAbsolute path where
    mkAbsolute :: (Seq PathNode) -> path

class FromParamName path where
    mkParamName :: PathNode -> path

class FromRelative path where
    mkRelative :: (Seq PathNode) -> path



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
    _relative :: SimplePrism path (Seq PathNode)

class ParamNamePrism path where
    _paramName :: SimplePrism path PathNode

class AbsolutePrism path where
    _absolute :: SimplePrism path (Seq PathNode)




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
