{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Kind
-- Description : The kinds of types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Kind
  ( Kind (..),
  _Value,
  _Stack,
  _Label,
  _Permission,
  (.:->)
  )
where

import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Optics.TH (makePrisms)

-- | A kind (κ) is the type of a type. Types with the \"value\" kind (@*@) are
-- inhabited by values; all other types are used only to enforce program
-- invariants. These include:
--
--  • The \"stack\" kind (ρ), used to enforce that the stack cannot contain
--    other stacks.
--
--  • The \"permission label\" kind (λ), used to identify a permission.
--
--  • The \"permission\" kind (ε), denoting a set of permissions.
--
--  • The \"function\" kind (κ → κ), used to describe type constructors.
data Kind = Value | Stack | Label | Permission | !Kind :-> !Kind
  deriving (Eq, Show)

makePrisms ''Kind

instance Hashable Kind where
  hashWithSalt s Value = hashWithSalt s (0 :: Int)
  hashWithSalt s Stack = hashWithSalt s (1 :: Int)
  hashWithSalt s Label = hashWithSalt s (2 :: Int)
  hashWithSalt s Permission = hashWithSalt s (3 :: Int)
  hashWithSalt s (a :-> b) = hashWithSalt s (4 :: Int, a, b)

instance Pretty Kind where
  pPrint Value = "value"
  pPrint Stack = "stack"
  pPrint Label = "label"
  pPrint Permission = "permission"
  pPrint (a :-> b) =
    Pretty.parens $
      Pretty.hsep
        [pPrint a, "->", pPrint b]
