{-# LANGUAGE TemplateHaskell #-}

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
  _Constructor,
  _Instance,
  _Permission,
  _Word
  )
where

import Relude
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Optics.TH (makePrisms)

data Category
  = Constructor
  | Instance
  | Permission
  | Word
  deriving (Eq, Show)

makePrisms ''Category

instance Pretty Category where
  pPrint Constructor = "constructor"
  pPrint Instance = "instance"
  pPrint Permission = "permission"
  pPrint Word = "word"