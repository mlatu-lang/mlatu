-- |
-- Module      : Mlatu.Instantiated
-- Description : Fully qualified instantiated names
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Instantiated
  ( Instantiated (..),
  )
where

import Mlatu.Name (Qualified)
import Mlatu.Type (Type)
import Relude hiding (Type)

data Instantiated = Instantiated
  { name :: !Qualified,
    types :: ![Type]
  }
  deriving (Ord, Eq, Show)

instance Hashable Instantiated where
  hashWithSalt s (Instantiated n ts) =
    hashWithSalt s (0 :: Int, n, ts)
