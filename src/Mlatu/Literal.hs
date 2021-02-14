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
    floatValue,
  )
where

import Data.Ratio ((%))
import Mlatu.Base (Base (..))
import Mlatu.Bits (FloatBits (..), IntegerBits (..))
import Numeric (showIntAtBase)
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data IntegerLiteral = IntegerLiteral
  { integerValue :: !Integer,
    integerBase :: !Base,
    integerBits :: !IntegerBits
  }
  deriving (Show)

-- Integer literals compare equality regardless of base and bits.
instance Eq IntegerLiteral where
  IntegerLiteral a _baseA _bitsA == IntegerLiteral b _baseB _bitsB = a == b

instance Pretty IntegerLiteral where
  pPrint literal =
    Pretty.hcat
      [ if value < 0 then "-" else "",
        case integerBase literal of
          Binary -> "0b"
          Octal -> "0o"
          Decimal -> ""
          Hexadecimal -> "0x",
        Pretty.text $ showIntAtBase base (\i -> Unsafe.fromJust (digits !!? i)) (abs value) "",
        case bits of
          Signed32 -> ""
          _nonDefault -> pPrint bits
      ]
    where
      value = integerValue literal
      bits = integerBits literal
      (base, digits) = case integerBase literal of
        Binary -> (2, "01")
        Octal -> (8, ['0' .. '7'])
        Decimal -> (10, ['0' .. '9'])
        Hexadecimal -> (16, ['0' .. '9'] ++ ['A' .. 'F'])

data FloatLiteral = FloatLiteral
  { floatSignificand :: !Integer,
    floatFractional :: !Int,
    floatExponent :: !Int,
    floatBits :: !FloatBits
  }
  deriving (Show)

-- Float literals compar equality regardless of bits.
instance Eq FloatLiteral where
  FloatLiteral a b c _bitsA == FloatLiteral d e f _bitsB = (a, c - b) == (d, f - e)

instance Pretty FloatLiteral where
  pPrint literal =
    Pretty.hcat
      [ if value < 0 then "-" else "",
        Pretty.double value,
        case bits of
          Float64 -> ""
          Float32 -> pPrint bits
      ]
    where
      bits = floatBits literal
      value = floatValue literal

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
floatValue (FloatLiteral a b c _bits) =
  let e = c - b
      -- The intermediate rational step is necessary to preserve precision.
      shift = if e < 0 then 1 % 10 ^ negate e else 10 ^ e
   in fromRational $ (fromIntegral a :: Rational) * shift
