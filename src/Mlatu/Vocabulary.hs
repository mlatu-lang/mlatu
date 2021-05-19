{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Mlatu.Vocabulary
-- Description : Namespaces
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Vocabulary
  ( pattern Global,
  )
where

import Mlatu.Name (Qualified (..), Qualifier (..), Root (..), Unqualified (..))
import Relude

pattern Global :: Unqualified -> Qualified
pattern Global u = Qualified (Qualifier Absolute []) u
