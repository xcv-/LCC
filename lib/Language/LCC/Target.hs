{-# LANGUAGE FlexibleContexts #-}
module Language.LCC.Target where

import Control.Applicative
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Writer (MonadWriter, censor, tell)

import qualified Data.DList as DL

import Data.Text.Format
import qualified Data.Text.Lazy as T

import Data.Text.Buildable as Buildable
import Data.Text.Format.Params

import Data.Monoid

import qualified Language.LCC.Error as Err
import Language.LCC.AST


class TargetConfig cfg where
    cfgTabWidth  :: cfg -> Int
    cfgExpandTab :: cfg -> Bool


class Target t where
    injectBuiltins :: (Err.ErrorM m, Applicative m)
                   => t
                   -> AbsLocale UnknownType
                   -> m (AbsLocale UnknownType)

    output :: (Err.ErrorM m)
           => t
           -> [AnalyzedLocale]
           -> m [(FilePath, T.Text)]

    inlineBuiltin :: Err.ErrorM m
                  => t -> AnalyzedSignature -> [AbsExpr] -> m AbsExpr
    inlineBuiltin _ sig args = return $ Funcall (Builtin sig) args


format1 :: Buildable b => Format -> b -> T.Text
format1 fmt = format fmt . Only

writeln :: MonadWriter (DL.DList a) m => a -> m ()
writeln l = tell (pure l)

writef1 :: (MonadWriter (DL.DList T.Text) m, Buildable b) => Format -> b -> m ()
writef1 fmt = writeln . format1 fmt

writefn :: (MonadWriter (DL.DList T.Text) m, Params ps) => Format -> ps -> m ()
writefn fmt = writeln . format fmt


prefixing :: MonadWriter (DL.DList T.Text) m => T.Text -> m a -> m a
prefixing prefix m =
    flip censor m . fmap $ \line ->
      if T.null line
        then line
        else prefix <> line

indent :: (TargetConfig cfg, MonadReader cfg m, MonadWriter (DL.DList T.Text) m)
       => m a
       -> m a
indent m = do
    unit <- munit
    prefixing unit m
  where
    munit :: (TargetConfig cfg, MonadReader cfg m) => m T.Text
    munit = do
      et <- asks cfgExpandTab

      if not et
        then return (T.pack "\t")
        else do
          tw <- asks (fromIntegral . cfgTabWidth)
          return $ T.replicate tw (T.pack " ")
