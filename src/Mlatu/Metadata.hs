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
  )
where

import Mlatu.Name (GeneralName, Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Term (Term)
import Relude

-- | Untyped metadata from @about@ blocks.
data Metadata = Metadata
  { fields :: !(HashMap Unqualified (Term ())),
    name :: !GeneralName,
    origin :: !Origin
  }
  deriving (Ord, Eq, Show)