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
  )
where

import Mlatu.Origin (Origin)
import Mlatu.Report (Report)
import Relude
import Text.PrettyPrint qualified as Pretty

-- | Class of error-reporting monads.
class (Monad m) => Informer m where
  -- | Halt if there are any fatal reports.
  checkpoint :: m ()

  -- | Halt the computation.
  halt :: m a

  -- | Add a report to the log.
  report :: Report -> m ()

  -- | Add local context to reports.
  while :: Origin -> Pretty.Doc -> m a -> m a
