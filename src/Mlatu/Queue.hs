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
    dequeue,
    Mlatu.Queue.empty,
    enqueue,
    Mlatu.Queue.fromList,
  )
where

import Relude (Maybe (..), reverse, (.))

-- | A generic queue with amortized O(1) enqueue/dequeue.
data Queue a = Queue ![a] ![a]

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue i (x : o)) = Just (x, Queue i o)
dequeue (Queue i@(_ : _) []) = dequeue (Queue [] (reverse i))
dequeue (Queue [] []) = Nothing

empty :: Queue a
empty = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue i o) = Queue (x : i) o

fromList :: [a] -> Queue a
fromList = Queue [] . reverse
