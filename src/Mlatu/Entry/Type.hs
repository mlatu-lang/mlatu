-- |
-- Module      : Mlatu.Entry.Type
-- Description : Type definition entries
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Entry.Type
  ( Entry (..),
  )
where

import Mlatu.Kind (Kind)
import Mlatu.Name (Qualified, Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Term (Term)
import Relude

data Entry = Entry
  -- The names of the constructors of this type.

  { constructors :: ![Qualified],
    -- Whether this type is visible outside its vocabulary.

    export :: !Bool,
    -- User-defined metadata.

    metadata :: !(Map Unqualified (Term ())),
    -- Source location.

    origin :: !Origin,
    -- Type parameters, for a generic definition.

    parameters :: ![(Unqualified, Kind, Origin)]
  }
  deriving (Show)
