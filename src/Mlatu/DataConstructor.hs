-- |
-- Module      : Mlatu.DataConstructor
-- Description : Constructors of data types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.DataConstructor
  ( DataConstructor (..),
  )
where

import Mlatu.Name (Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Pretty as Pretty
import Mlatu.Signature (Signature)
import Relude
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | A single data constructor case, e.g., @case some (T)@.
data DataConstructor = DataConstructor
  { fields :: ![Signature],
    name :: !Unqualified,
    origin :: !Origin
  }
  deriving (Eq, Ord, Show)

-- FIXME: Support fields.
instance Pretty DataConstructor where
  pPrint (DataConstructor fields name _) =
    "case"
      <+> pPrint name
      <+> Pretty.parens (Pretty.list (map pPrint fields))
