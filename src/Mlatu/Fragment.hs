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
    intrinsics,
    classes,
    wordDefinitions,
    constructorDefinitions,
    metadata,
    instances,
    types,
  )
where

import Mlatu.Class (Class (..))
import Mlatu.Definition (ConstructorDefinition, WordDefinition)
import Mlatu.Instance (Instance)
import Mlatu.Intrinsic (Intrinsic (..))
import Mlatu.Metadata (Metadata)
import Mlatu.TypeDefinition (TypeDefinition)
import Optics
import Relude

-- | A program fragment, consisting of a bag of top-level program elements.
data Fragment a = Fragment
  { _intrinsics :: ![Intrinsic],
    _instances :: ![Instance a],
    _classes :: ![Class],
    _wordDefinitions :: ![WordDefinition a],
    _constructorDefinitions :: ![ConstructorDefinition a],
    _metadata :: ![Metadata],
    _types :: ![TypeDefinition]
  }
  deriving (Show)

makeLenses ''Fragment

instance Monoid (Fragment a) where
  mempty =
    Fragment
      { _intrinsics = mempty,
        _instances = mempty,
        _classes = mempty,
        _wordDefinitions = mempty,
        _metadata = mempty,
        _constructorDefinitions = mempty,
        _types = mempty
      }

instance Semigroup (Fragment a) where
  a <> b =
    Fragment
      { _intrinsics = _intrinsics a <> _intrinsics b,
        _instances = _instances a <> _instances b,
        _classes = _classes a <> _classes b,
        _wordDefinitions = _wordDefinitions a <> _wordDefinitions b,
        _metadata = _metadata a <> _metadata b,
        _constructorDefinitions = _constructorDefinitions a <> _constructorDefinitions b,
        _types = _types a <> _types b
      }
