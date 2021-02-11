-- |
-- Module      : Mlatu.Layoutness
-- Description : Whether a block is a layout block
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Layoutness
  ( Layoutness (..),
  )
where

import Relude

-- | A display hint for whether a block was originally written with 'Layout'
-- (@:@) or 'Nonlayout' (@{}@) syntax.
data Layoutness = Layout | Nonlayout
  deriving (Show)
