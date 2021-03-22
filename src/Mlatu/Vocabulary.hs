-- |
-- Module      : Mlatu.Vocabulary
-- Description : Namespaces
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Vocabulary
  ( global,
    intrinsic,
    intrinsicName,
  )
where

import Mlatu.Name (Qualifier (..), Root (..))
import Relude

global :: Qualifier
global = Qualifier Absolute []

intrinsic :: Qualifier
intrinsic = Qualifier Absolute [intrinsicName]

intrinsicName :: Text
intrinsicName = "mlatu"
