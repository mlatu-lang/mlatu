{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Fragment
-- Description : Program fragments
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Fragment
  ( Fragment (..),
    traits,
    definitions,
    metadata,
    types,
  )
where

import Mlatu.Definition (Definition)
import Mlatu.Metadata (Metadata)
import Mlatu.Trait (Trait (..))
import Mlatu.TypeDefinition (TypeDefinition)
import Optics
import Relude

-- | A program fragment, consisting of a bag of top-level program elements.
data Fragment a = Fragment
  { _traits :: ![Trait],
    _definitions :: ![Definition a],
    _metadata :: ![Metadata],
    _types :: ![TypeDefinition]
  }
  deriving (Show)

makeLenses ''Fragment

instance Monoid (Fragment a) where
  mempty =
    Fragment
      { _traits = mempty,
        _definitions = mempty,
        _metadata = mempty,
        _types = mempty
      }

instance Semigroup (Fragment a) where
  (<>) a =
    over traits (<> view traits a)
      . over definitions (<> view definitions a)
      . over metadata (<> view metadata a)
      . over types (<> view types a)
