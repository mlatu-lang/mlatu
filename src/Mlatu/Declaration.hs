-- |
-- Module      : Mlatu.Declaration
-- Description : Declarations of intrinsics and traits
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Declaration
  ( Category (..),
    Declaration (..),
  )
where

import Mlatu.Name (Qualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | The type of declaration.
data Category
  = -- | @intrinsic@, a built-in/external function.
    Intrinsic
  | -- | @trait@, a generic function.
    Trait
  deriving (Eq, Show)

instance Pretty Category where
  pPrint Intrinsic = "intrinsic"
  pPrint Trait = "trait"

data Declaration = Declaration
  { category :: !Category,
    name :: !Qualified,
    origin :: !Origin,
    signature :: !Signature
  }
  deriving (Show)

instance Pretty Declaration where
  pPrint (Declaration category name _ signature) = pPrint category Pretty.<+> pPrint (unqualifiedName name) Pretty.<+> pPrint signature
