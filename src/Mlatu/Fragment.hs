{-# LANGUAGE StrictData #-}
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
    declarations,
    definitions,
    metadata,
    types,
  )
where

import Mlatu.Declaration (Declaration (..))
import Mlatu.Definition (Definition)
import Mlatu.Metadata (Metadata)
import Mlatu.TypeDefinition (TypeDefinition)
import Optics
import Relude

-- | A program fragment, consisting of a bag of top-level program elements.
data Fragment a = Fragment
  { _declarations :: [Declaration],
    _definitions :: [Definition a],
    _metadata :: [Metadata],
    _types :: [TypeDefinition]
  }
  deriving (Show)

makeLenses ''Fragment

instance Monoid (Fragment a) where
  mempty =
    Fragment
      { _declarations = mempty,
        _definitions = mempty,
        _metadata = mempty,
        _types = mempty
      }

instance Semigroup (Fragment a) where
  a <> b =
    over
      declarations
      (<> view declarations b)
      ( over
          definitions
          (<> view definitions b)
          ( over
              metadata
              (<> view metadata b)
              (over types (<> view types b) a)
          )
      )
