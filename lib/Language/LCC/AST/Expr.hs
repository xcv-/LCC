{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.AST.Expr where

import Control.Applicative
import Control.Lens

import Data.Foldable hiding (all)
import Data.Monoid

import Language.LCC.AST.Path
import Language.LCC.AST.Signature


data FuncallPath path
    = Builtin AnalyzedSignature
    | Input PathNode
    | Fn path
    deriving (Eq, Ord, Show)

makePrisms ''FuncallPath


data Expr path
    = IntL    Integer
    | DoubleL Double
    | BoolL   Bool
    | CharL   Char
    | StringL String
    | SConcat [Expr path]
    | Array   [Expr path]

    | Funcall (FuncallPath path) [Expr path]

    | Cond    (Expr path) (Expr path) (Expr path)
    deriving (Eq, Ord, Show)

makePrisms ''Expr

type RelExpr = Expr RelativeVarPath
type AbsExpr = Expr AbsoluteVarPath
type RawExpr = RelExpr


isLiteral :: Expr path -> Bool
isLiteral (IntL _)    = True
isLiteral (DoubleL _) = True
isLiteral (BoolL _)   = True
isLiteral (CharL _)   = True
isLiteral (StringL _) = True
isLiteral (Array xs)  = all isLiteral xs
isLiteral _           = False


instance Functor FuncallPath where
    fmap f (Fn p)        = Fn (f p)
    fmap _ (Builtin sig) = Builtin sig

instance Foldable FuncallPath where
    foldMap f (Fn p) = f p
    foldMap _ _      = mempty

instance Traversable FuncallPath where
    traverse f (Fn p)        = fmap Fn (f p)
    traverse _ (Builtin sig) = pure $ Builtin sig


instance Functor Expr where
    fmap _ (IntL x)            = IntL x
    fmap _ (DoubleL x)         = DoubleL x
    fmap _ (BoolL x)           = BoolL x
    fmap _ (CharL x)           = CharL x
    fmap _ (StringL x)         = StringL x
    fmap f (SConcat ss)        = SConcat $ (fmap.fmap) f ss
    fmap f (Array xs)          = Array $ (fmap.fmap) f xs
    fmap f (Funcall path args) = Funcall (fmap f path) $ (fmap.fmap) f args
    fmap f (Cond cond ifT ifF) = Cond (fmap f cond) (fmap f ifT) (fmap f ifF)


instance Foldable Expr where
    foldMap f (SConcat ss)        = (foldMap.foldMap) f ss
    foldMap f (Array xs)          = (foldMap.foldMap) f xs
    foldMap f (Funcall path args) = foldMap f path <> (foldMap.foldMap) f args
    foldMap f (Cond cond ifT ifF) = foldMap f cond <> foldMap f ifT
                                                   <> foldMap f ifF
    foldMap _ _ = mempty


instance Traversable Expr where
    traverse _ (IntL x)            = pure $ IntL x
    traverse _ (DoubleL x)         = pure $ DoubleL x
    traverse _ (BoolL x)           = pure $ BoolL x
    traverse _ (CharL x)           = pure $ CharL x
    traverse _ (StringL x)         = pure $ StringL x

    traverse f (SConcat ss)        = SConcat <$> (traverse.traverse) f ss

    traverse f (Array xs)          = Array <$> (traverse.traverse) f xs

    traverse f (Funcall path args) = Funcall <$> traverse f path
                                             <*> (traverse.traverse) f args

    traverse f (Cond cond ifT ifF) = Cond <$> traverse f cond
                                          <*> traverse f ifT
                                          <*> traverse f ifF
