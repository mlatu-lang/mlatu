-- |
-- Module      : Mlatu.Entry
-- Description : Dictionary entries
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Middle.Entry
  ( WordEntry (..),
    MetadataEntry (..),
    TypeEntry (..),
    TraitEntry (..),
  )
where

import Mlatu.Base.Name (Unqualified)
import Mlatu.Base.Origin (Origin)
import Mlatu.Base.Type (Type)
import Mlatu.Front.Definition (Category (..), Merge (..), Parent (..))
import Mlatu.Front.Parameter (Parameter)
import Mlatu.Front.Signature (Signature)
import Mlatu.Front.Term (Term)

data WordEntry = WordEntry !Category !Merge !Origin !(Maybe Parent) !(Maybe Signature) !(Maybe (Term Type))
  deriving (Show)

data MetadataEntry = MetadataEntry !Origin !(Term ())
  deriving (Show)

data TypeEntry = TypeEntry !Origin ![Parameter] ![(Unqualified, [Signature], [Signature], Origin)]
  deriving (Show)

data TraitEntry = TraitEntry !Origin !Signature
  deriving (Show)
