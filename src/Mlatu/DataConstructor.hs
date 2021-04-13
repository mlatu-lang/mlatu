{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.DataConstructor
-- Description : Constructors of data types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.DataConstructor
  ( DataConstructor (..),
    fields,
    name,
    origin,
  )
where

import Mlatu.Name (Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Optics.TH (makeLenses)
import Relude

-- | A single data constructor case, e.g., @case some (T)@.
data DataConstructor = DataConstructor
  { _fields :: [Signature],
    _name :: Unqualified,
    _origin :: Origin
  }
  deriving (Eq, Ord, Show)

makeLenses ''DataConstructor
