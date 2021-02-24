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

import Data.List (groupBy)
import Mlatu.Declaration (Declaration (..))
import Mlatu.Definition (Definition)
import Mlatu.Metadata (Metadata)
import Mlatu.Name (Qualified (qualifierName), Qualifier (..), Root (..))
import Mlatu.Pretty qualified as Pretty
import Mlatu.Synonym (Synonym)
import Mlatu.TypeDefinition (TypeDefinition)
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..), (<+>))

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

instance (Show a, Ord a) => Pretty (Fragment a) where
  pPrint fragment =
    Pretty.vsep $
      concat
        [ map printGrouped groupedDeclarations,
          map pPrint $ sort (synonyms fragment),
          map pPrint $ sort (types fragment),
          map pPrint $ sort (definitions fragment),
          map pPrint $ sort (metadata fragment)
        ]
    where
      groupedDeclarations = groupBy (\a b -> (qualifierName . name) a == (qualifierName . name) b) (declarations fragment)
      printGrouped decls =
        if noVocab
          then Pretty.vcat (map pPrint $ sort decls)
          else Pretty.block ("vocab" <+> pPrint commonName) (Pretty.vcat (map pPrint $ sort decls))
        where
          (commonName, noVocab) = case qualifierName $ name $ decls Unsafe.!! 0 of
            (Qualifier Absolute parts) -> (Qualifier Relative parts, null parts)
            n -> (n, False)
