{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Declaration
-- Description : Declarations of intrinsics and traits
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.Trait
  ( Trait (..),
    name,
    origin,
    signature,
  )
where

import Mlatu.Base.Name (Qualified (..))
import Mlatu.Base.Origin (Origin)
import Mlatu.Front.Signature (Signature)

data Trait = Trait
  { _name :: !Qualified,
    _origin :: !Origin,
    _signature :: !Signature
  }
  deriving (Eq, Ord, Show)

makeLenses ''Trait
