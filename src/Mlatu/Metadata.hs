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
    name,
    fields,
    origin
  )
where


import Control.Lens (makeLenses, (^.))
import Data.HashMap.Strict qualified as HashMap
import Mlatu.Name (GeneralName, Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Term (Term)
import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | Untyped metadata from @about@ blocks.
data Metadata = Metadata
  { _fields :: !(HashMap Unqualified (Term ())),
    _name :: !GeneralName,
    _origin :: !Origin
  }
  deriving (Show)

makeLenses ''Metadata

instance Pretty Metadata where
  pPrint metadata =
    Pretty.vcat
      [ Pretty.hcat ["about ", pPrint $ metadata ^. name, ":"],
        Pretty.nest 4 $
          Pretty.vcat $
            map field $
              HashMap.toList $ metadata ^. fields
      ]
    where
      field (key, value) =
        Pretty.vcat
          [ Pretty.hcat [pPrint key, ":"],
            Pretty.nest 4 $ pPrint value
          ]
