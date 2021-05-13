-- |
-- Module      : Mlatu.Entry
-- Description : Dictionary entries
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Entry
  ( WordEntry (..),
    MetadataEntry (..),
    TypeEntry (..),
    TypeAliasEntry (..),
    TraitEntry (..),
  )
where

import Mlatu.DataConstructor (DataConstructor)
import Mlatu.Entry.Category (Category)
import Mlatu.Entry.Merge (Merge)
import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Entry.Parent (Parent)
import Mlatu.Name (Qualified)
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Mlatu.Term (Term)
import Mlatu.Type (Type)
import Optics.TH (makePrisms)
import Relude hiding (Constraint, Type)

data WordEntry = WordEntry !Category !Merge !Origin !(Maybe Parent) !(Maybe Signature) !(Maybe (Term Type))
  deriving (Show)

data MetadataEntry = MetadataEntry !Origin !(Term ())
  deriving (Show)

data TypeEntry = TypeEntry !Origin ![Parameter] ![DataConstructor]
  deriving (Show)

data TypeAliasEntry = TypeAliasEntry !Origin !Qualified
  deriving (Show)

data TraitEntry = TraitEntry !Origin !Signature
  deriving (Show)
