{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module Language.LCC.Types.Path where

import GHC.Exts (IsList(..))

import Control.Applicative
import Control.Lens

import qualified Data.Foldable as Foldable
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



-- Instances


instance IsList (Seq.Seq a) where
    type Item (Seq.Seq a) = a
    toList = Foldable.toList
    fromList = Seq.fromList

instance IsList AbsolutePath where
    type Item AbsolutePath = PathNode
    toList = toList . view absolute
    fromList = view (from absolute) . fromList


defaultShow = mconcat . intersperse "." . toList

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

absolute :: Iso' AbsolutePath (Seq PathNode)
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



class FromRelative path where
    _Relative :: Prism' path (Seq PathNode)

class FromParamName path where
    _ParamName :: Prism' path PathNode

class FromAbsolute path where
    _Absolute :: Prism' path (Seq PathNode)




instance FromRelative RelativePath where
    _Relative = prism RRelativePath $ \case
        RRelativePath x -> Right x
        x               -> Left x

instance FromRelative RelativeVarPath where
    _Relative = prism RVRelativePath $ \case
        RVRelativePath x -> Right x
        x                -> Left x


instance FromParamName RelativeVarPath where
    _ParamName = prism RVParamName $ \case
        RVParamName x -> Right x
        x             -> Left x

instance FromParamName AbsoluteVarPath where
    _ParamName = prism VParamName $ \case
        VParamName x -> Right x
        x            -> Left x


instance FromAbsolute RelativePath where
    _Absolute = prism RAbsolutePath $ \case
        RAbsolutePath x -> Right x
        x               -> Left x

instance FromAbsolute AbsolutePath where
    _Absolute = prism AbsolutePath $ Right . view absolute


instance FromAbsolute RelativeVarPath where
    _Absolute = prism RVAbsolutePath $ \case
        RVAbsolutePath x -> Right x
        x                -> Left x

instance FromAbsolute AbsoluteVarPath where
    _Absolute = prism VAbsolutePath $ \case
        VAbsolutePath x -> Right x
        x               -> Left x
