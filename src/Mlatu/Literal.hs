{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Literal
-- Description : Representations of literal values
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Literal
  ( FloatLiteral (..),
    IntegerLiteral (..),
    Base (..),
    integerValue,
    integerBase,
    floatValue,
    floatSignificand,
    floatFractional,
    floatExponent,
  )
where

import Data.Ratio ((%))
import Optics.TH
import Relude

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
  deriving (Ord, Eq, Show)

makePrisms ''Base

data IntegerLiteral = IntegerLiteral
  { _integerValue :: Integer,
    _integerBase :: Base
  }
  deriving (Show)

makeLenses ''IntegerLiteral

-- Integer literals compare equality regardless of base and bits.
instance Eq IntegerLiteral where
  IntegerLiteral a _baseA == IntegerLiteral b _baseB = a == b

deriving instance Ord IntegerLiteral

data FloatLiteral = FloatLiteral
  { _floatSignificand :: Integer,
    _floatFractional :: Int,
    _floatExponent :: Int
  }
  deriving (Show)

makeLenses ''FloatLiteral

-- Float literals compar equality regardless of bits.
instance Eq FloatLiteral where
  FloatLiteral a b c == FloatLiteral d e f = (a, c - b) == (d, f - e)

deriving instance Ord FloatLiteral

-- Note [Float Literals]:
--
-- Floating-point literals are represented as a pair of an arbitrary-precision
-- integer significand and exponent, so that:
--
--     Float a b c
--
-- Denotes the floating point number (a × 10^(c - b)). This representation was
-- chosen to avoid loss of precision until the token is converted into a machine
-- floating-point format. The exponent is split into two parts to indicate which
-- part of the literal that exponent came from: the fractional part, or the
-- exponent in scientific notation.

floatValue :: Fractional a => FloatLiteral -> a
floatValue (FloatLiteral a b c) =
  let e = c - b
      -- The intermediate rational step is necessary to preserve precision.
      shift = if e < 0 then 1 % 10 ^ negate e else 10 ^ e
   in fromRational $ (fromIntegral a :: Rational) * shift
