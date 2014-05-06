{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simplify
  ( simplify
  ) where

import Control.Lens
import qualified Data.Map as Map


data EvalEnv t = EvalEnv { _localeImpl :: LocaleImpl
                         , _target :: t
                         }


$(makeLens ''EvalEnv)


newtype Eval t a = Eval (StateT (EitherT LC Expr) (EvalEnv t) a)
  deriving ( Functor, Applicative, Monad
           , MonadState (EvalEnv t)
           , MonadState LocaleState
           , MonadError Expr
           , MonadError LocaleError
           )


simplify :: Target t => t -> Locale -> LC Locale
simplify

eval :: Target t => Expr -> Eval t Expr
eval expr@(IntLiteral _)    = return expr
eval expr@(DoubleLiteral _) = return expr
eval expr@(BoolLiteral _)   = return expr
eval expr@(CharLiteral _)   = return expr
eval expr@(StringLiteral _) = return expr
eval expr@(StringConcat xs) = StringLiteral <$> mapM eval xs
eval expr@(ArrayLiteral xs) = ArrayLiteral <$> mapM eval xs

eval expr@(Conditional cond true false) = do
    cond <- eval cond
    if cond
        then eval true
        else eval false

eval expr@(Funcall path args) = do
    types <- mapM inferType args
    env <- get

    case env^.localeImpl.lciEnv.at (path, types) of

        Just (params,body) ->
            evalFunctionBody path params body

        Nothing -> do
            args <- mapM eval args

            evalBuiltin (env^.target) path args
                & either throwError return
