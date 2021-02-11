-- |
-- Module      : Mlatu.Bits
-- Description : Fixed-size Mlatu types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Bits
  ( FloatBits (..),
    IntegerBits (..),
  )
where

import Relude
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | Standard sizes of fixed-precision floating-point numbers.
data FloatBits
  = -- | @f32@
    Float32
  | -- | @f64@
    Float64
  deriving (Eq, Show)

-- | Standard sizes of fixed-precision integer numbers.
data IntegerBits
  = -- | @i8@
    Signed8
  | Signed16
  | Signed32
  | Signed64
  | Unsigned8
  | Unsigned16
  | Unsigned32
  | Unsigned64
  deriving (Eq, Show)

instance Pretty IntegerBits where
  pPrint bits = case bits of
    Signed8 -> "i8"
    Signed16 -> "i16"
    Signed32 -> "i32"
    Signed64 -> "i64"
    Unsigned8 -> "u8"
    Unsigned16 -> "u16"
    Unsigned32 -> "u32"
    Unsigned64 -> "u64"

instance Pretty FloatBits where
  pPrint bits = case bits of
    Float32 -> "f32"
    Float64 -> "f64"
