-- |
-- Module      : Mlatu.Synonym
-- Description : Aliases
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Synonym
  ( Synonym (..),
  )
where

import Mlatu.Name (GeneralName, Qualified)
import Mlatu.Origin (Origin)
import Relude
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data Synonym = Synonym !Qualified !GeneralName !Origin
  deriving (Show)

-- FIXME: Real instance.
instance Pretty Synonym where
  pPrint (Synonym name1 name2 _) = pPrint name1 <> "=" <> pPrint name2
