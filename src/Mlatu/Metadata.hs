{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Metadata
-- Description : Metadata about identifiers in the dictionary
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Metadata
  ( Metadata (..),
    fields,
    name,
    origin,
  )
where

import Mlatu.Name (GeneralName, Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Term (Term)
import Optics.TH
import Relude

-- | Untyped metadata from @about@ blocks.
data Metadata = Metadata
  { _fields :: (Map Unqualified (Term ())),
    _name :: GeneralName,
    _origin :: Origin
  }
  deriving (Ord, Eq, Show)

makeLenses ''Metadata
