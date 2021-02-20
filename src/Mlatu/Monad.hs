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
  {unKT :: Context -> Reports -> m (Either Reports (a, Reports))}

type Context = [(Origin, Pretty.Doc)]

type Reports = [Report]

type M = MT IO

-- | Runs a nested action, returning whether it completed successfully, that is,
-- without generating any reports.
attempt :: (Monad m) => MT m a -> MT m Bool
attempt action = MT $ \context reports -> do
  mr <- unKT action context reports
  case mr of
    Left reports' -> return $ Right (False, reports')
    Right (_, reports') -> return $ Right (True, reports')

-- | Runs an action, returning the accumulated reports (if any) or final result.
runMlatu :: (Monad m) => MT m a -> m (Either [Report] a)
runMlatu (MT m) = do
  mr <- m [] []
  case mr of
    Left reports -> return $ Left reports
    Right (result, _) -> return $ Right result

instance (Monad m) => Functor (MT m) where
  fmap f (MT ax) = MT $ \context reports -> do
    mr <- ax context reports
    case mr of
      Left reports' -> return $ Left reports'
      Right (x, reports') -> return $ Right (f x, reports')

instance (Monad m) => Applicative (MT m) where
  pure x = MT $ const (return . Right . (x,))
  MT af <*> MT ax = MT $ \context reports -> do
    mf <- af context reports
    case mf of
      Right (f, reports') -> do
        mx <- ax context reports'
        case mx of
          Right (x, reports'') -> return $ Right (f x, reports'')
          Left reports'' -> return $ Left reports''
      Left reports' -> return $ Left reports'

instance (Monad m) => Monad (MT m) where
  return = pure
  MT ax >>= f = MT $ \context reports -> do
    mx <- ax context reports
    case mx of
      Left reports' -> return $ Left reports'
      Right (x, reports') -> unKT (f x) context reports'

instance Monad m => MonadFail (MT m) where
  fail = error "do not use 'fail'"

instance (MonadIO m) => MonadFix (MT m) where
  mfix k = MT $ \context reports -> do
    m <- liftIO newEmptyMVar
    a <- liftIO $ unsafeInterleaveIO $ takeMVar m
    mx <- unKT (k a) context reports
    case mx of
      Left {} -> return mx
      Right (x, _) -> do
        liftIO $ putMVar m x
        return mx

instance (MonadIO m) => MonadIO (MT m) where
  liftIO m = MT $ \_context reports -> do
    x <- liftIO m
    return $ Right (x, reports)

instance (Monad m) => Informer (MT m) where
  checkpoint lvls = MT $ \_context reports ->
    return $
      if not (any (\(Report lvl _) -> lvl `elem` lvls) reports) then Right ((), reports) else Left reports
  halt = MT $ \_context reports -> return $ Left reports
  report r = MT $ \context reports ->
    return . Right . (,) () $
      ( \case
          [] -> r : reports
          ctxt -> Report Info (Context ctxt r) : reports
      )
        context
  while origin message action = MT $ \context reports ->
    unKT action ((origin, message) : context) reports
