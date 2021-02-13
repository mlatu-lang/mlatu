{-# LANGUAGE TemplateHaskell #-}

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
    associativity,
    name,
    precedence,
  )
where

import Control.Lens (makeLenses, (^.))
import Mlatu.Name (Qualified)
import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

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
  deriving (Show)

-- | The precedence level (from 0 to 9) of an operator; higher-precedence
-- operators bind more tightly than lower-precedence operators.
newtype Precedence = Precedence Int
  deriving (Enum, Eq, Ord, Show, Pretty)

instance Bounded Precedence where
  minBound = Precedence 0
  maxBound = Precedence 9

-- | Operator metadata for infix desugaring.
data Operator = Operator
  { _associativity :: !Associativity,
    _name :: !Qualified,
    _precedence :: !Precedence
  }
  deriving (Show)

makeLenses ''Operator

-- | Whether a word was declared infix (@+@) or postfix (@plus@).
data Fixity = Infix | Postfix
  deriving (Eq, Show)

instance Pretty Operator where
  pPrint operator =
    Pretty.hsep $
      ("infix" :) $
        ( case operator ^. associativity of
            Nonassociative -> id
            Leftward -> ("left" :)
            Rightward -> ("right" :)
        )
          [ pPrint $ operator ^. precedence,
            pPrint $ operator ^. name
          ]
