{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Intrinsic
-- Description : Declarations of intrinsics and traits
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Intrinsic
  ( Intrinsic (..),
    name,
    origin,
    signature,
  )
where

import Mlatu.Name (Qualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Optics.TH (makeLenses)
import Relude

data Intrinsic = Intrinsic
  { _name :: !Qualified,
    _origin :: !Origin,
    _signature :: !Signature
  }
  deriving (Eq, Ord, Show)

makeLenses ''Intrinsic
