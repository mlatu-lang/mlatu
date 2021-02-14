{-# LANGUAGE TemplateHaskell #-}

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
  _Layout,
  _Nonlayout
  )
where

import Relude
import Optics.TH (makePrisms)

-- | A display hint for whether a block was originally written with 'Layout'
-- (@:@) or 'Nonlayout' (@{}@) syntax.
data Layoutness = Layout | Nonlayout
  deriving (Show)

makePrisms ''Layoutness