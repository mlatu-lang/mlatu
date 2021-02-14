{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Located
-- Description : Imbuing a value with a location
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Located
  ( Located (..),
    origin,
    indent,
    item,
  )
where

import Mlatu.Indent (Indent)
import Mlatu.Origin (Origin)
import Optics.TH (makeLenses)
import Relude
import Text.Show qualified

-- | Imbues a value (such as a 'Token') with an origin and indent level.
data Located a = At
  { _origin :: !Origin,
    _indent :: !Indent,
    _item :: !a
  }

makeLenses ''Located

instance (Show a) => Show (Located a) where
  show = show . _item
