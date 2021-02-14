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
  ( Fragment,
    declarations,
    definitions,
    metadata,
    synonyms,
    types,
  )
where

import Mlatu.Declaration (Declaration)
import Mlatu.Definition (Definition)
import Mlatu.Metadata (Metadata)
import Mlatu.Pretty qualified as Pretty
import Mlatu.Synonym (Synonym)
import Mlatu.TypeDefinition (TypeDefinition)
import Optics (over, makeLenses, view)
import Relude
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | A program fragment, consisting of a bag of top-level program elements.
data Fragment a = Fragment
  { _declarations :: ![Declaration],
    _definitions :: ![Definition a],
    _metadata :: ![Metadata],
    _synonyms :: ![Synonym],
    _types :: ![TypeDefinition]
  }
  deriving (Show)

makeLenses ''Fragment

instance Monoid (Fragment a) where
  mempty =
    Fragment
      { _declarations = mempty,
        _definitions = mempty,
        _metadata = mempty,
        _synonyms = mempty,
        _types = mempty
      }
  mappend = (<>)

instance Semigroup (Fragment a) where
  (<>) a =
    over declarations (<> _declarations a)
      . over definitions (<> _definitions a)
      . over metadata (<> _metadata a)
      . over synonyms (<> _synonyms a)
      . over types (<> _types a)

instance Pretty (Fragment a) where
  pPrint fragment =
    Pretty.vsep $
      concat
        [ map pPrint $ view definitions fragment,
          map pPrint $ view metadata fragment,
          map pPrint $ view synonyms fragment,
          map pPrint $ view types fragment
        ]
