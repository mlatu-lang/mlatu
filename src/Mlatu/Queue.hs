{-# LANGUAGE StrictData #-}

-- |
-- Module      : Mlatu.Queue
-- Description : Queue utilities
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Queue
  ( Queue,
    fromList,
  )
where

import Relude hiding (fromList)

-- | A generic queue with amortized O(1) enqueue/dequeue.
data Queue a = Queue [a] [a]

fromList :: [a] -> Queue a
fromList = Queue [] . reverse
