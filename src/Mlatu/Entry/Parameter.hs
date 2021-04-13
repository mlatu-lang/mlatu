{-# LANGUAGE StrictData #-}

-- |
-- Module      : Mlatu.Entry.Parameter
-- Description : Type parameters
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Entry.Parameter
  ( Parameter (..),
  )
where

import Mlatu.Kind (Kind (..))
import Mlatu.Name (Unqualified)
import Mlatu.Origin (Origin)
import Relude

-- | A generic type parameter for a data type, like @T@ in @List[T]@.
data Parameter = Parameter Origin Unqualified Kind
  deriving (Ord, Show)

-- | Parameters are compared regardless of origin.
instance Eq Parameter where
  Parameter _ a b == Parameter _ c d = (a, b) == (c, d)
