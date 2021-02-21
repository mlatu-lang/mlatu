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
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data Category
  = Constructor
  | Instance
  | Permission
  | Word
  deriving (Ord, Eq, Show)

instance Pretty Category where
  pPrint Constructor = "constructor"
  pPrint Instance = "instance"
  pPrint Permission = "permission"
  pPrint Word = "word"