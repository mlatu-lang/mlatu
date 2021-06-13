{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Fragment
-- Description : Program fragments
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.Fragment
  ( Fragment (..),
    traits,
    definitions,
    metadata,
    dataDefinitions,
    codataDefinitions,
  )
where

import Mlatu.Front.CodataDefinition (CodataDefinition)
import Mlatu.Front.DataDefinition (DataDefinition)
import Mlatu.Front.Definition (Definition)
import Mlatu.Front.Metadata (Metadata)
import Mlatu.Front.Trait (Trait (..))

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
