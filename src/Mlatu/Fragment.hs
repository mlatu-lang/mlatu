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
    dataDefinitions,
    codataDefinitions,
  )
where

import Mlatu.CodataDefinition (CodataDefinition)
import Mlatu.DataDefinition (DataDefinition)
import Mlatu.Definition (Definition)
import Mlatu.Metadata (Metadata)
import Mlatu.Trait (Trait (..))
import Optics
import Relude

-- | A program fragment, consisting of a bag of top-level program elements.
data Fragment a = Fragment
  { _traits :: ![Trait],
    _definitions :: ![Definition a],
    _metadata :: ![Metadata],
    _dataDefinitions :: ![DataDefinition],
    _codataDefinitions :: ![CodataDefinition]
  }
  deriving (Show)

makeLenses ''Fragment

instance Monoid (Fragment a) where
  mempty =
    Fragment
      { _traits = mempty,
        _definitions = mempty,
        _metadata = mempty,
        _dataDefinitions = mempty,
        _codataDefinitions = mempty
      }

instance Semigroup (Fragment a) where
  (<>) a =
    over traits (<> view traits a)
      . over definitions (<> view definitions a)
      . over metadata (<> view metadata a)
      . over dataDefinitions (<> view dataDefinitions a)
      . over codataDefinitions (<> view codataDefinitions a)
