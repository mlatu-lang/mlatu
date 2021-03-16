-- |
-- Module      : Mlatu.Monad
-- Description : Error-reporting I/O monad
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Monad
  ( M,
    MT,
    attempt,
    runMlatu,
    runMlatuExceptT,
  )
where

import Control.Monad (ap)
import Control.Monad.Fix (MonadFix (..))
import Mlatu.Informer (Informer (..))
import Mlatu.Origin (Origin)
import Mlatu.Report (Level (..), Report (..), ReportKind (..))
import Prettyprinter (Doc)
import Relude
import System.IO.Unsafe (unsafeInterleaveIO)
import Mlatu.Ice (ice)

-- | A Mlatu action atop a 'Monad' 'm', returning a result of type 'a', which
-- maintains a 'Context' stack and can fail with a list of 'Reports'.
newtype MT m a = MT
  {unKT :: Context -> Reports -> ExceptT Reports m (a, Reports)}

type Context = [(Origin, Doc ())]

type Reports = [Report]

type M = MT IO

-- | Runs a nested action, returning whether it completed successfully, that is,
-- without generating any reports.
attempt :: (Monad m) => MT m a -> MT m Bool
attempt action = MT $ \context reports ->
  ExceptT $
    runExceptT (unKT action context reports)
      <&> Right . either (False,) ((True,) . snd)

-- | Runs an action, returning the accumulated reports (if any) or final result.
runMlatu :: (Monad m) => MT m a -> ExceptT [Report] m a
runMlatu (MT m) = m [] [] <&> fst

runMlatuExceptT :: (Monad m) => MT m a -> m (Either [Report] a)
runMlatuExceptT = runExceptT . runMlatu

instance (Monad m) => Functor (MT m) where
  fmap f (MT ax) = MT $ flip flip (first f) . ((<&>) .) . ax
  {-# INLINEABLE fmap #-}

instance (Monad m) => Applicative (MT m) where
  pure x = MT $ const (return . (x,))
  MT af <*> MT ax = MT $ ap (flip . ((>>=) .) . af) (uncurry . (. first) . flip . ((<&>) .) . ax)
  {-# INLINEABLE (<*>) #-}

instance (Monad m) => Monad (MT m) where
  return = pure
  MT ax >>= f = MT $ ap (flip . ((>>=) .) . ax) (uncurry . (. f) . flip unKT)
  {-# INLINEABLE (>>=) #-}

instance Monad m => MonadFail (MT m) where
  fail = ice "Mlatu.Monad.fail - 'fail' was used in code"

instance (MonadIO m) => MonadFix (MT m) where
  mfix k = MT $ \context reports ->
    newEmptyMVar
      >>= ap
        ((>>=) . liftIO . unsafeInterleaveIO . takeMVar)
        ((<=< flip (flip unKT context . k) reports) . (liftIO .) . (`ap` return) . ((>>) .) . (. fst) . putMVar)

instance (MonadIO m) => MonadIO (MT m) where
  liftIO m = MT $ const ((liftIO m <&>) . flip (,))

instance (Monad m) => Informer (MT m) where
  checkpoint minLvl = MT $ \_ reports ->
    hoistEither $
      if all (\(Report lvl _) -> lvl < minLvl) reports
        then Right ((), reports)
        else Left reports
  halt = MT $ const (hoistEither . Left)
  report r = MT $ \context reports ->
    return . (,) () $
      (\ctxt -> (if null ctxt then r else Report Info (Context ctxt r)) : reports)
        context
  while origin message action = MT $ unKT action . ((origin, message) :)
