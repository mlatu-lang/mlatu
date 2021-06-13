{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Mlatu.Vocabulary
-- Description : Namespaces
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Base.Vocabulary
  ( pattern Global,
  )
where

import Mlatu.Base.Name (Qualified (..), Qualifier (..), Root (..), Unqualified (..))

pattern Global :: Unqualified -> Qualified
pattern Global u = Qualified (Qualifier Absolute []) u
