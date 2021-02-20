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

import Data.HashMap.Strict qualified as HashMap
import Mlatu.Name (GeneralName, Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Term (Term)
import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | Untyped metadata from @about@ blocks.
data Metadata = Metadata
  { fields :: !(HashMap Unqualified (Term ())),
    name :: !GeneralName,
    origin :: !Origin
  }
  deriving (Show)

instance Pretty Metadata where
  pPrint metadata =
    Pretty.vcat
      [ Pretty.hcat ["about ", pPrint $ name metadata, ":"],
        Pretty.nest 2 $
          Pretty.vcat $
            map field $
              HashMap.toList $
                fields metadata
      ]
    where
      field (key, value) =
        Pretty.vcat
          [ Pretty.hcat [pPrint key, ":"],
            Pretty.nest 2 $ pPrint value
          ]
