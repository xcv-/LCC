{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Simplifier.Inline where

import Control.Lens
import Control.Monad (liftM, liftM3)
import Control.Monad.Reader (MonadReader, runReaderT, asks, local)

import Data.List (find)
import Data.Maybe

import Language.LCC.AST
import Language.LCC.Target
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err


type InliningEnv t m = (Target t, MonadReader (CallStack t) m)

type CallStack t = (AnalyzedAST, [CallStackFrame], Int, t)
type CallStackFrame = (AbsolutePath, [BoundParam])
type BoundParam = (Param, AbsExpr)



inline :: (Functor m, Err.ErrorM m, Target t)
       => Int -> t -> AnalyzedAST -> m AnalyzedAST
inline maxStackDepth t ast =
    flip runReaderT (ast, [], maxStackDepth, t) $
      scopedMapM (trImpl inlineExpr) ast


inlineExpr :: (Err.ErrorM m, ScopedAbs Type m, InliningEnv t m)
           => AbsExpr
           -> m AbsExpr
inlineExpr expr = case expr of
    Array arr  -> liftM Array (mapM inlineExpr arr)
    SConcat s  -> liftM (concatLits . SConcat) (mapM inlineExpr s)

    Cond c t f -> do
      let (c',t',f') = over each inlineExpr (c,t,f)

      c' >>= \case
        BoolL b -> if b then t' else f'
        _       -> liftM3 Cond c' t' f'

    Funcall fn args -> do
      t <- getTarget

      inlinedArgs <- mapM inlineExpr args

      case fn of
        Builtin sig          -> inlineBuiltin t sig inlinedArgs
        Input _ _            -> return expr
        Fn (VAbsolutePath p) -> inlineFuncall (p^.from absolute) inlinedArgs
        Fn (VParamName name) -> fromMaybe expr `liftM` findBoundParam name

    _ -> return expr
  where
    concatLits :: AbsExpr -> AbsExpr
    concatLits = \case
        SConcat [s] -> concatLits s
        SConcat (s:ss) ->
          case (concatLits s, concatLits (SConcat ss)) of
            (StringL s', SConcat ss') -> SConcat $ StringL s' : ss'
            (StringL s', StringL ss') -> StringL $ s' ++ ss'
            (SConcat s', SConcat ss') -> SConcat $ s' ++ ss'
            (s', ss')                 -> SConcat [s',ss']

        expr' -> expr'


findBoundParam :: InliningEnv t m => String -> m (Maybe AbsExpr)
findBoundParam name = do
    boundParams <- asks (^.._2.traverse._2.traverse)

    return . fmap snd $ find (paramNameEq name . fst) boundParams


inlineFuncall :: (Err.ErrorM m, ScopedAbs Type m, InliningEnv t m)
              => AbsolutePath
              -> [AbsExpr]
              -> m AbsExpr
inlineFuncall f args = do
    let vf = f^.absolute.re _Absolute
        noop = return $ Funcall (Fn vf) args

    availStack <- getAvailStack

    if availStack == 0
      then noop
      else do
        ast  <- getAST
        tr   <- findFunction ast f args
        uRec <- isUndRec (tr^.trSig) args

        if uRec
          then noop
          else extendStack (bindParams tr args)
                           (tr ./> inlineExpr . view trImpl)


isUndRec :: (Err.ErrorM m, ScopedAbs Type m, InliningEnv t m)
         => AnalyzedSignature -> [AbsExpr] -> m Bool
isUndRec callee args = do
    caller <- viewS trSig

    return $ caller == callee && all (has $ _Funcall._1._Fn._ParamName) args


extendStack :: InliningEnv t m => CallStackFrame -> m a -> m a
extendStack frame = local $ over _2 (frame:)
                          . over _3 (subtract 1)

bindParams :: AbsTranslation Type -> [AbsExpr] -> CallStackFrame
bindParams tr args = let sig = tr^.trSig
                     in (sig^.sigPath, zip (sig^.sigParams) args)


getAST :: InliningEnv t m => m AnalyzedAST
getAST = view _1

getAvailStack :: InliningEnv t m => m Int
getAvailStack = view _3

getTarget :: InliningEnv t m => m t
getTarget = view _4
