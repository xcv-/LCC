{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Language.LCC.AST.AST where

import qualified Data.Map.Strict as Map

import Language.LCC.AST.Path
import Language.LCC.AST.Signature
import Language.LCC.AST.Translation

import Data.TaggedTree


type AST path ret = TaggedTree PathNode (Translation path ret)

type RelAST ret = AST RelativeVarPath ret
type AbsAST ret = AST AbsoluteVarPath ret
type RawAST = RelAST UnknownType
type AnalyzedAST = AbsAST Type

type FlatAST path ret = [(AbsolutePath, [Translation path ret])]
type FlatRelAST ret = FlatAST RelativeVarPath ret
type FlatAbsAST ret = FlatAST AbsoluteVarPath ret

type FlatASTMap path ret = Map.Map AbsolutePath [Translation path ret]
type FlatRelASTMap ret = FlatASTMap RelativeVarPath ret
type FlatAbsASTMap ret = FlatASTMap AbsoluteVarPath ret
