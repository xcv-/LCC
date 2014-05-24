{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Simplifier.Inline where

import Control.Applicative
import Control.Lens
import Control.Monad (liftM, liftM2, liftM3)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks, local)

import qualified Data.Map as Map

import Language.LCC.Types
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err



type CallStack = (AbsAST Type, [CallStackFrame])
type CallStackFrame = (AbsolutePath, [BoundParam])
type BoundParam = (Param, AbsExpr)



inline :: (Functor m, Err.ErrorM m) => AbsAST Type -> m (AbsAST Type)
inline ast = flip runReaderT (ast, []) $ scopedMapM (trImpl %%~ inlineExpr) ast


inlineExpr :: (Err.ErrorM m, ScopedAbs Type m, MonadReader CallStack m)
           => AbsExpr
           -> m AbsExpr
inlineExpr expr
  | is _Array   = liftM Array   $ mapM inlineExpr (expr^?!_Array)
  | is _SConcat = liftM SConcat $ mapM inlineExpr (expr^?!_SConcat)
  | is _Cond    =
      let (c,t,f) = expr^?!_Cond
      in liftM3 Cond (inlineExpr c) (inlineExpr t) (inlineExpr f)

  | is _Funcall = inlineFuncall expr (expr^?!_Funcall)

  | otherwise = return expr

  where
    is prism = has prism expr


inlineFuncall :: (Err.ErrorM m, ScopedAbs Type m, MonadReader CallStack m)
              => AbsExpr
              -> (AbsoluteVarPath, [AbsExpr])
              -> m AbsExpr
inlineFuncall expr (f,args) =
    case f of
      VParamName _paramName -> do
        _paramType <- getAST >>= typeOf expr
        boundParams <- getBoundParams

        getBoundParam boundParams Param {..}

      VAbsolutePath p -> do
        ast <- getAST

        case findTrans ast (p^.from absolute) of
          Nothing -> Err.symbolNotFound f

          Just tr
            | has (trImpl._Builtin) tr -> return expr
            | otherwise -> do
                inlinedArgs <- mapM inlineExpr args
                extendStack (bindParams tr inlinedArgs)
                            (tr ./> inlineExpr . view trImpl)



getBoundParams :: MonadReader CallStack m => m [BoundParam]
getBoundParams = asks (concat . map snd . snd)

getBoundParam :: (Err.ErrorM m, ScopedAbs Type m)
              => [BoundParam]
              -> Param
              -> m AbsExpr
getBoundParam boundParams param = do
    case lookup param boundParams of
      Just expr ->
        return expr
      Nothing ->
        Err.signatureNotFound (VParamName $ param^.paramName) []


extendStack :: MonadReader CallStack m => CallStackFrame -> m a -> m a
extendStack frame = local $ _2 %~ (frame:)

bindParams :: AbsTranslation Type -> [AbsExpr] -> CallStackFrame
bindParams tr args = let sig = tr^.trSig
                     in (sig^.sigPath, zip (sig^.sigParams) args)


findTrans :: AST path ret -> AbsolutePath -> Maybe (Translation path ret)
findTrans ast path = ast ^? atPath path

getAST :: MonadReader CallStack m => m (AbsAST Type)
getAST = asks fst
