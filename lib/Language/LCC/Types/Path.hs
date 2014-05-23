{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module Language.LCC.Types.Path where

import Control.Applicative
import Control.Comonad
import Control.Lens

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Monoid

import qualified Data.Sequence as Seq


type PathNode = String
type Seq a = Seq.Seq a


data RelativePath = RAbsolutePath (Seq PathNode)
                  | RRelativePath (Seq PathNode)
    deriving (Eq, Ord)

newtype AbsolutePath = AbsolutePath (Seq PathNode)
    deriving (Eq, Ord, Monoid)


data RelativeVarPath = RVAbsolutePath (Seq PathNode)
                     | RVRelativePath (Seq PathNode)
                     | RVParamName    PathNode
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
    mkRelative = RVRelativePath


instance FromParamName RelativeVarPath  where
    mkParamName = RVParamName

instance FromParamName AbsoluteVarPath where
    mkParamName = VParamName


instance FromAbsolute RelativeVarPath where
    mkAbsolute = RVAbsolutePath

instance FromAbsolute AbsoluteVarPath where
    mkAbsolute = VAbsolutePath

instance FromAbsolute AbsolutePath where
    mkAbsolute = AbsolutePath




-- Instances

defaultShow = show . mconcat . intersperse "." . toList

instance Show RelativePath where
    show (RRelativePath path) = defaultShow path
    show (RAbsolutePath path) = defaultShow path

instance Show AbsolutePath where
    show (AbsolutePath path) = defaultShow path


instance Show RelativeVarPath where
    show (RVRelativePath path) = defaultShow path
    show (RVAbsolutePath path) = defaultShow path
    show (RVParamName name)    = "@" <> name

instance Show AbsoluteVarPath where
    show (VAbsolutePath path) = defaultShow path
    show (VParamName name)    = "@" <> name



-- Lens

absolute :: Iso' AbsolutePath (Seq.Seq PathNode)
absolute = iso (\(AbsolutePath path) -> path) AbsolutePath



-- Prisms

type Cons' s a = Cons s s a a


instance Cons AbsolutePath AbsolutePath PathNode PathNode where
    _Cons = prism prepend $ \(AbsolutePath nns) -> case Seq.viewl nns of
        n Seq.:< ns -> Right (n, AbsolutePath ns)
        _           -> Left (AbsolutePath nns)
      where
        prepend (n, AbsolutePath ns) = AbsolutePath (n <| ns)


instance Snoc AbsolutePath AbsolutePath PathNode PathNode where
    _Snoc = prism append $ \(AbsolutePath nns) -> case Seq.viewr nns of
        ns Seq.:> n -> Right (AbsolutePath ns, n)
        _           -> Left (AbsolutePath nns)
      where
        append (AbsolutePath ns, n) = AbsolutePath (ns |> n)



class RelativePrism path where
    _Relative :: Prism' path (Seq PathNode)

class ParamNamePrism path where
    _ParamName :: Prism' path PathNode

class AbsolutePrism path where
    _Absolute :: Prism' path (Seq PathNode)




instance RelativePrism RelativePath where
    _Relative = prism RRelativePath $ \case
        RRelativePath x -> Right x
        x               -> Left x

instance RelativePrism RelativeVarPath where
    _Relative = prism RVRelativePath $ \case
        RVRelativePath x -> Right x
        x                -> Left x


instance ParamNamePrism RelativeVarPath where
    _ParamName = prism RVParamName $ \case
        RVParamName x -> Right x
        x             -> Left x

instance ParamNamePrism AbsoluteVarPath where
    _ParamName = prism VParamName $ \case
        VParamName x -> Right x
        x            -> Left x


instance AbsolutePrism RelativePath where
    _Absolute = prism RAbsolutePath $ \case
        RAbsolutePath x -> Right x
        x               -> Left x

instance AbsolutePrism AbsolutePath where
    _Absolute = prism AbsolutePath $ Right . view absolute


instance AbsolutePrism RelativeVarPath where
    _Absolute = prism RVAbsolutePath $ \case
        RVAbsolutePath x -> Right x
        x                -> Left x

instance AbsolutePrism AbsoluteVarPath where
    _Absolute = prism VAbsolutePath $ \case
        VAbsolutePath x -> Right x
        x               -> Left x
