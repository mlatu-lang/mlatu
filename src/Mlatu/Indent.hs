-- |
-- Module      : Mlatu.Indent
-- Description : Indent levels
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Indent
  ( Indent (..),
  )
where

import Relude
import Text.Parsec (Column)

-- | The indent level of a token, defined as the first column of the first token
-- in the same line.
newtype Indent = Indent Column
  deriving (Eq, Ord, Show)
