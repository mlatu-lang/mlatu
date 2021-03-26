-- |
-- Module      : Mlatu.Informer
-- Description : Error-reporting monad
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Informer
  ( Informer (..),
    errorCheckpoint,
    warnCheckpoint,
    infoCheckpoint,
  )
where

import Mlatu.Origin (Origin)
import Mlatu.Report (Level (..), Report)
import Prettyprinter (Doc)
import Relude

-- | Class of error-reporting monads.
class (Monad m) => Informer m where
  -- | Halt if there are any fatal reports.
  checkpoint :: Level -> m ()

  -- | Halt the computation.
  halt :: m a

  -- | Add a report to the log.
  report :: Report -> m ()

  -- | Add local context to reports.
  while :: Origin -> Doc () -> m a -> m a

errorCheckpoint :: (Informer m) => m ()
errorCheckpoint = checkpoint Error

warnCheckpoint :: (Informer m) => m ()
warnCheckpoint = checkpoint Warn

infoCheckpoint :: (Informer m) => m ()
infoCheckpoint = checkpoint Info
