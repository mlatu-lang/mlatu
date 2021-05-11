-- |
-- Module      : Mlatu.Entry.Parent
-- Description : Links to parent entries
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Entry.Parent
  ( Parent (..),
  )
where

import Mlatu.Name (Qualified)
import Relude

-- | A parent trait (of an instance) or data type (of a constructor).
data Parent
  = Trait !Qualified
  | Type !Qualified
  | Record !Qualified
  deriving (Ord, Eq, Show)
