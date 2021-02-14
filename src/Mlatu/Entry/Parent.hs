{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Entry.Parent
-- Description : Links to parent entries
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Entry.Parent
  ( Parent (..),
  _Trait,
  _Type,
  )
where

import Mlatu.Name (Qualified)
import Mlatu.Pretty qualified as Pretty
import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Optics.TH (makePrisms)

-- | A parent trait (of an instance) or data type (of a constructor).
data Parent
  = Trait !Qualified
  | Type !Qualified
  deriving (Show)

makePrisms ''Parent

instance Pretty Parent where
  pPrint (Trait name) = Pretty.hsep ["trait", Pretty.quote name]
  pPrint (Type name) = Pretty.hsep ["type", Pretty.quote name]
