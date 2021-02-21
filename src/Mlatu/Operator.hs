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
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

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
  deriving (Enum, Eq, Ord, Show, Pretty)

instance Bounded Precedence where
  minBound = Precedence 0
  maxBound = Precedence 9

instance Pretty Operator where
  pPrint operator =
    Pretty.hsep $
      ("infix" :) $
        ( case associativity operator of
            Nonassociative -> id
            Leftward -> ("left" :)
            Rightward -> ("right" :)
        )
          [ pPrint $ precedence operator,
            pPrint $ name operator
          ]
