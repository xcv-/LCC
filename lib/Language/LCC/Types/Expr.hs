{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.Types.Expr where

import Control.Applicative
import Control.Lens

import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Traversable

import Language.LCC.Types.Signature


data Expr path
    = IntL    { _exprInt      :: Integer     }
    | DoubleL { _exprDouble   :: Double      }
    | BoolL   { _exprBool     :: Bool        }
    | CharL   { _exprChar     :: Char        }
    | StringL { _exprString   :: String      }
    | Builtin { _exprBtInType :: Type        }
    | SConcat { _exprStrings  :: [Expr path] }
    | Array   { _exprArray    :: [Expr path] }

    | Funcall { _exprFunction :: path
              , _exprArgs     :: [Expr path]
              }

    | Cond    { _exprCondition   :: Expr path
              , _exprConsequent  :: Expr path
              , _exprAlternative :: Expr path
              }
    deriving (Ord, Eq, Show)


instance Functor Expr where
    fmap f (IntL x)            = IntL x
    fmap f (DoubleL x)         = DoubleL x
    fmap f (BoolL x)           = BoolL x
    fmap f (CharL x)           = CharL x
    fmap f (StringL x)         = StringL x
    fmap f (Builtin t)         = Builtin t
    fmap f (SConcat ss)        = SConcat $ (fmap.fmap) f ss
    fmap f (Array xs)          = Array $ (fmap.fmap) f xs
    fmap f (Funcall path args) = Funcall (f path) $ (fmap.fmap) f args
    fmap f (Cond cond ifT ifF) = Cond (fmap f cond) (fmap f ifT) (fmap f ifF)


instance Foldable Expr where
    foldMap f (SConcat ss)        = (foldMap.foldMap) f ss
    foldMap f (Array xs)          = (foldMap.foldMap) f xs
    foldMap f (Funcall path args) = f path <> (foldMap.foldMap) f args
    foldMap f (Cond cond ifT ifF) = foldMap f cond <> foldMap f ifT
                                                   <> foldMap f ifF
    foldMap _ _ = mempty


instance Traversable Expr where
    traverse f (IntL x)            = pure $ IntL x
    traverse f (DoubleL x)         = pure $ DoubleL x
    traverse f (BoolL x)           = pure $ BoolL x
    traverse f (CharL x)           = pure $ CharL x
    traverse f (StringL x)         = pure $ StringL x
    traverse f (Builtin t)         = pure $ Builtin t

    traverse f (SConcat ss)        = SConcat <$> (traverse.traverse) f ss

    traverse f (Array xs)          = Array <$> (traverse.traverse) f xs

    traverse f (Funcall path args) = Funcall <$> f path
                                             <*> (traverse.traverse) f args

    traverse f (Cond cond ifT ifF) = Cond <$> traverse f cond
                                          <*> traverse f ifT
                                          <*> traverse f ifF



makeLenses ''Expr
makePrisms ''Expr
