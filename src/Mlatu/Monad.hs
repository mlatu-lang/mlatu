-- |
-- Module      : Mlatu.Monad
-- Description : Error-reporting I/O monad
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Monad
  ( K,
    KT,
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
newtype KT m a = KT
  {unKT :: Context -> Reports -> m (Either Reports (a, Reports))}

type Context = [(Origin, Pretty.Doc)]

type Reports = [Report]

type K = KT IO

-- | Runs a nested action, returning whether it completed successfully, that is,
-- without generating any reports.
attempt :: (Monad m) => KT m a -> KT m Bool
attempt action = KT $ \context reports -> do
  mr <- unKT action context reports
  case mr of
    Left reports' -> return $ Right (False, reports')
    Right (_, reports') -> return $ Right (True, reports')

-- | Runs an action, returning the accumulated reports (if any) or final result.
runMlatu :: (Monad m) => KT m a -> m (Either [Report] a)
runMlatu (KT m) = do
  mr <- m [] []
  case mr of
    Left reports -> return $ Left reports
    Right (result, _) -> return $ Right result

instance (Monad m) => Functor (KT m) where
  fmap f (KT ax) = KT $ \context reports -> do
    mr <- ax context reports
    case mr of
      Left reports' -> return $ Left reports'
      Right (x, reports') -> return $ Right (f x, reports')

instance (Monad m) => Applicative (KT m) where
  pure x = KT $ \_context reports -> return $ Right (x, reports)
  KT af <*> KT ax = KT $ \context reports -> do
    mf <- af context reports
    case mf of
      Right (f, reports') -> do
        mx <- ax context reports'
        case mx of
          Right (x, reports'') -> return $ Right (f x, reports'')
          Left reports'' -> return $ Left reports''
      Left reports' -> return $ Left reports'

instance (Monad m) => Monad (KT m) where
  return x = KT $ \_context reports -> return $ Right (x, reports)
  KT ax >>= f = KT $ \context reports -> do
    mx <- ax context reports
    case mx of
      Left reports' -> return $ Left reports'
      Right (x, reports') -> unKT (f x) context reports'

instance Monad m => MonadFail (KT m) where
  fail = error "do not use 'fail'"

instance (MonadIO m) => MonadFix (KT m) where
  mfix k = KT $ \context reports -> do
    m <- liftIO newEmptyMVar
    a <- liftIO $ unsafeInterleaveIO $ takeMVar m
    mx <- unKT (k a) context reports
    case mx of
      Left {} -> return mx
      Right (x, _) -> do
        liftIO $ putMVar m x
        return mx

instance (MonadIO m) => MonadIO (KT m) where
  liftIO m = KT $ \_context reports -> do
    x <- liftIO m
    return $ Right (x, reports)

instance (Monad m) => Informer (KT m) where
  checkpoint lvls = KT $ \_context reports ->
    return $
      if not (any (\(Report lvl _) -> lvl `elem` lvls) reports) then Right ((), reports) else Left reports
  halt = KT $ \_context reports -> return $ Left reports
  report r = KT $ \context reports ->
    return . Right . (,) () $
      ( \case
          [] -> r : reports
          ctxt -> Report Info (Context ctxt r) : reports
      )
        context
  while origin message action = KT $ \context reports ->
    unKT action ((origin, message) : context) reports
