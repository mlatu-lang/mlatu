{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Base
-- Description : Numeric literal bases
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Base
  ( Base (..),
  )
where

import Relude
import Optics.TH (makePrisms)

-- | The radix of an integer literal.
data Base
  = -- | @0b@
    Binary
  | -- | @0o@
    Octal
  | -- | No prefix.
    Decimal
  | -- | @0x@
    Hexadecimal
  deriving (Show)

makePrisms ''Base