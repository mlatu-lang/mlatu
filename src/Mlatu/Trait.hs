{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Declaration
-- Description : Declarations of intrinsics and traits
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Trait
  ( Trait (..),
    name,
    origin,
    signature,
  )
where

import Mlatu.Name (Qualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Optics.TH (makeLenses, makePrisms)
import Relude

data Trait = Trait
  { _name :: !Qualified,
    _origin :: !Origin,
    _signature :: !Signature
  }
  deriving (Eq, Ord, Show)

makeLenses ''Trait
