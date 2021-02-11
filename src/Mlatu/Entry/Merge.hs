-- |
-- Module      : Mlatu.Entry.Merge
-- Description : Merge behavior for dictionary entries
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Entry.Merge
  ( Merge (..),
  )
where

import Relude

-- | When adding a definition to the dictionary, if an existing definition has
-- the same name, the default 'Merge' behavior of 'Deny' raises an error, while
-- 'Compose' composes the bodies of the two definitions.
data Merge = Deny | Compose
  deriving (Eq, Show)
