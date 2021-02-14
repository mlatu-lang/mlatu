{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Instantiated
-- Description : Fully qualified instantiated names
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Instantiated
  ( Instantiated (..),
  name,
  types,
  )
where

import Mlatu.Name (Qualified)
import Mlatu.Pretty qualified as Pretty
import Mlatu.Type (Type)
import Relude hiding (Type)
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Optics.TH (makeLenses)

data Instantiated = Instantiated
  { _name :: !Qualified,
    _types :: ![Type]
  }
  deriving (Eq, Show)

makeLenses ''Instantiated

instance Hashable Instantiated where
  hashWithSalt s (Instantiated n ts) =
    hashWithSalt s (0 :: Int, n, ts)

instance Pretty Instantiated where
  pPrint (Instantiated n ts) =
    Pretty.hcat
      [pPrint n, "::[", Pretty.list $ map pPrint ts, "]"]
