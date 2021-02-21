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
  )
where

import Control.Monad (ap)
import Control.Monad.Fix (MonadFix (..))
import Mlatu.Informer (Informer (..))
import Mlatu.Origin (Origin)
import Mlatu.Report (Level (..), Report (..), ReportKind (..))
import Relude
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.PrettyPrint qualified as Pretty

-- | A Mlatu action atop a 'Monad' 'm', returning a result of type 'a', which
-- maintains a 'Context' stack and can fail with a list of 'Reports'.
newtype MT m a = MT
  {unKT :: Context -> Reports -> ExceptT Reports m (a, Reports)}

type Context = [(Origin, Pretty.Doc)]

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

instance (Monad m) => Functor (MT m) where
  fmap f (MT ax) = MT $ flip flip (first f) . ((<&>) .) . ax

instance (Monad m) => Applicative (MT m) where
  pure x = MT $ const (return . (x,))
  MT af <*> MT ax = MT $ ap (flip . ((>>=) .) . af) (uncurry . (. first) . flip . ((<&>) .) . ax)

instance (Monad m) => Monad (MT m) where
  return = pure
  MT ax >>= f = MT $ 
    ap (flip . ((>>=) .) . ax) (uncurry . (. f) . flip unKT)
    
instance Monad m => MonadFail (MT m) where
  fail = error "do not use 'fail'"

instance (MonadIO m) => MonadFix (MT m) where
  mfix k = MT $ \context reports -> do
    m <- liftIO newEmptyMVar
    a <- liftIO $ unsafeInterleaveIO $ takeMVar m
    mx@(x, _) <- unKT (k a) context reports
    liftIO $ putMVar m x
    return mx

instance (MonadIO m) => MonadIO (MT m) where
  liftIO m = MT $ const ((liftIO m <&>) . flip (,))

instance (Monad m) => Informer (MT m) where
  checkpoint lvls = MT $ \_ reports ->
    hoistEither $
      if not (any (\(Report lvl _) -> lvl `elem` lvls) reports)
        then Right ((), reports)
        else Left reports
  halt = MT $ const (hoistEither . Left)
  report r = MT $ \context reports ->
    return . (,) () $
      ( \case
          [] -> r : reports
          ctxt -> Report Info (Context ctxt r) : reports
      )
        context
  while origin message action = MT $ \context reports ->
    unKT action ((origin, message) : context) reports
