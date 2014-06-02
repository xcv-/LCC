{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Simplifier.Inline where

import Debug.Trace

import Control.Applicative
import Control.Lens
import Control.Monad (liftM, liftM2, liftM3)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks, local)

import Data.Functor.Reverse
import Data.List (find)
import Data.Maybe
import qualified Data.Map as Map

import Language.LCC.AST
import Language.LCC.Pretty
import Language.LCC.TypeChecker
import qualified Language.LCC.Error as Err



type CallStack = (AnalyzedAST, [CallStackFrame], Int)
type CallStackFrame = (AbsolutePath, [BoundParam])
type BoundParam = (Param, AbsExpr)



inline :: (Functor m, Err.ErrorM m) => Int -> AnalyzedAST -> m AnalyzedAST
inline maxStackDepth ast =
    flip runReaderT (ast, [], maxStackDepth) $
      scopedMapM (trImpl inlineExpr) ast


inlineExpr :: (Err.ErrorM m, ScopedAbs Type m, MonadReader CallStack m)
           => AbsExpr
           -> m AbsExpr
inlineExpr expr
  | is _Array   = liftM Array   $ mapM inlineExpr (expr^?!_Array)
  | is _SConcat = liftM SConcat $ mapM inlineExpr (expr^?!_SConcat)
  | is _Cond    = do
      let (c,t,f) = over each inlineExpr (expr^?!_Cond)

      c >>= \case
        BoolL b -> if b then t else f
        _       -> liftM3 Cond c t f

  | is _Funcall =
      case expr^?!_Funcall of
        (VAbsolutePath p, args) -> inlineFuncall (p^.from absolute) args
        (VParamName name, args) -> liftM (fromMaybe expr) (findBoundParam name)

  | otherwise   = return expr
  where
    is prism = has prism expr


findBoundParam :: MonadReader CallStack m => String -> m (Maybe AbsExpr)
findBoundParam name = do
    boundParams <- asks (^.._2 . to Reverse . traverse . _2 . traverse)

    return . fmap snd $ find (paramNameEq name . fst) boundParams


inlineFuncall :: (Err.ErrorM m, ScopedAbs Type m, MonadReader CallStack m)
              => AbsolutePath
              -> [AbsExpr]
              -> m AbsExpr
inlineFuncall f args = do
    let vf   = f^.absolute.re _Absolute

    ast <- getAST
    tr  <- findFunction ast f args

    if has (trImpl._Builtin) tr
      then return (Funcall vf args)
      else do
        inlinedArgs <- mapM inlineExpr args
        availStack <- getAvailStack

        if availStack == 0
          then return (Funcall vf inlinedArgs)
          else extendStack (bindParams tr inlinedArgs)
                           (tr ./> inlineExpr . view trImpl)




extendStack :: MonadReader CallStack m => CallStackFrame -> m a -> m a
extendStack frame = local $ over _2 (frame:)
                          . over _3 (subtract 1)

bindParams :: AbsTranslation Type -> [AbsExpr] -> CallStackFrame
bindParams tr args = let sig = tr^.trSig
                     in (sig^.sigPath, zip (sig^.sigParams) args)


getAST :: MonadReader CallStack m => m AnalyzedAST
getAST = view _1

getAvailStack :: MonadReader CallStack m => m Int
getAvailStack = view _3
