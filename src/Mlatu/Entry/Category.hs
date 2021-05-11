-- |
-- Module      : Mlatu.Entry.Category
-- Description : Types of dictionary entries
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Entry.Category
  ( Category (..),
  )
where

import Relude

data Category
  = Constructor
  | Instance
  | Permission
  | Word
  | Deconstructor
  deriving (Ord, Eq, Show)
