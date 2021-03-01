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

-- | The type of declaration.
data Category
  = -- | @intrinsic@, a built-in/external function.
    Intrinsic
  | -- | @trait@, a generic function.
    Trait
  deriving (Eq, Ord, Show)

data Declaration = Declaration
  { category :: !Category,
    name :: !Qualified,
    origin :: !Origin,
    signature :: !Signature
  }
  deriving (Eq, Ord, Show)
