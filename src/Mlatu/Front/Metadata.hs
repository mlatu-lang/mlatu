{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Metadata
-- Description : Metadata about identifiers in the dictionary
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.Metadata
  ( Metadata (..),
    fields,
    name,
    origin,
  )
where

import Mlatu.Base.Name (GeneralName, Unqualified)
import Mlatu.Base.Origin (Origin)
import Mlatu.Front.Term (Term)

-- | Untyped metadata from @about@ blocks.
data Metadata = Metadata
  { _fields :: !(Map Unqualified (Term ())),
    _name :: !GeneralName,
    _origin :: !Origin
  }
  deriving (Ord, Eq, Show)

makeLenses ''Metadata
