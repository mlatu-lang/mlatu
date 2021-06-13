{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Located
-- Description : Imbuing a value with a location
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Base.Located
  ( Located (..),
  )
where

import Mlatu.Base.Origin (Origin)
import Text.Show qualified

-- | Imbues a value (such as a 'Token') with an origin and indent level.
data Located a = At
  { origin :: !Origin,
    item :: !a
  }

makeLenses ''Located

instance (Show a) => Show (Located a) where
  show = show . item
