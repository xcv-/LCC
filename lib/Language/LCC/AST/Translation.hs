{-# LANGUAGE TemplateHaskell #-}
module Language.LCC.AST.Translation where

import Control.Lens

import Text.Parsec (SourcePos)

import Language.LCC.AST.Annotation
import Language.LCC.AST.Expr
import Language.LCC.AST.Path
import Language.LCC.AST.Signature



data Translation path ret =
    Translation { _trSig         :: Signature AbsolutePath ret
                , _trImpl        :: Expr path
                , _trAnnotations :: [Annotation]
                , _trSourcePos   :: SourcePos
                }
  deriving (Eq, Show)

makeLenses ''Translation


type RelTranslation ret = Translation RelativeVarPath ret
type AbsTranslation ret = Translation AbsoluteVarPath ret
type RawTranslation = RelTranslation UnknownType
type AnalyzedTranslation = AbsTranslation Type


isBuiltinTr :: Translation path ret -> Bool
isBuiltinTr = any isBuiltin . view trAnnotations

isPrivateTr :: Translation path ret -> Bool
isPrivateTr = any isPrivate . view trAnnotations

isPublicTr :: Translation path ret -> Bool
isPublicTr = not . isPrivateTr


trParamTypes :: Traversal' (Translation path ret) Type
trParamTypes = trSig.sigParamTypes


matchTrParams :: Eq path => Translation path ret -> Translation path ret -> Bool
matchTrParams t1 t2 = matchSig (t1^.trSig) (t2^.trSig)


lookupParam :: Translation path ret -> PathNode -> Maybe Param
lookupParam t name =
    t^?trSig.sigParams.folded.filtered (\p -> p^.paramName == name)
