-- |
-- Module      : Mlatu.Operator
-- Description : Infix operator metadata
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Operator
  ( Associativity (..),
    Fixity (..),
    Operator (..),
    Precedence (..),
  )
where

import Mlatu.Name (Qualified)
import Relude

-- | Operator metadata for infix desugaring.
data Operator = Operator
  { associativity :: !Associativity,
    name :: !Qualified,
    precedence :: !Precedence
  }
  deriving (Ord, Eq, Show)

-- | Whether a word was declared infix (@+@) or postfix (@plus@).
data Fixity = Infix | Postfix
  deriving (Ord, Eq, Show)

-- | Whether an operator associates leftward:
--
-- > a + b + c = (a + b) + c
--
-- Rightward:
--
-- > a + b + c = a + (b + c)
--
-- Or not at all:
--
-- > a + b + c  // error
data Associativity = Nonassociative | Leftward | Rightward
  deriving (Ord, Eq, Show)

-- | The precedence level (from 0 to 9) of an operator; higher-precedence
-- operators bind more tightly than lower-precedence operators.
newtype Precedence = Precedence Int
  deriving (Enum, Eq, Ord, Show)

instance Bounded Precedence where
  minBound = Precedence 0
  maxBound = Precedence 9
