{-# LANGUAGE TemplateHaskell #-}

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
    category,
    name,
    origin,
    signature,
    _Intrinsic,
    _Trait,
  )
where

import Mlatu.Name (Qualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Optics.TH (makeLenses, makePrisms)
import Relude

-- | The type of declaration.
data Category
  = -- | @intrinsic@, a built-in/external function.
    Intrinsic
  | -- | @trait@, a generic function.
    Trait
  deriving (Eq, Ord, Show)

makePrisms ''Category

data Declaration = Declaration
  { _category :: !Category,
    _name :: !Qualified,
    _origin :: !Origin,
    _signature :: !Signature
  }
  deriving (Eq, Ord, Show)

makeLenses ''Declaration
