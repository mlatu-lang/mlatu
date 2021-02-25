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
  )
where

import Mlatu.Declaration (Declaration (..))
import Mlatu.Definition (Definition)
import Mlatu.Metadata (Metadata)
import Mlatu.Synonym (Synonym)
import Mlatu.TypeDefinition (TypeDefinition)
import Relude

-- | A program fragment, consisting of a bag of top-level program elements.
data Fragment a = Fragment
  { declarations :: ![Declaration],
    definitions :: ![Definition a],
    metadata :: ![Metadata],
    synonyms :: ![Synonym],
    types :: ![TypeDefinition]
  }
  deriving (Show)

instance Monoid (Fragment a) where
  mempty =
    Fragment
      { declarations = mempty,
        definitions = mempty,
        metadata = mempty,
        synonyms = mempty,
        types = mempty
      }

instance Semigroup (Fragment a) where
  (<>) a b =
    Fragment
      { declarations = declarations a <> declarations b,
        definitions = definitions a <> definitions b,
        metadata = metadata a <> metadata b,
        synonyms = synonyms a <> synonyms b,
        types = types a <> types b
      }