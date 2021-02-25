-- |
-- Module      : Mlatu.TypeDefinition
-- Description : Definitions of types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.TypeDefinition
  ( TypeDefinition (..),
  )
where

import Mlatu.DataConstructor (DataConstructor)
import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (Qualified)
import Mlatu.Origin (Origin)
import Mlatu.Signature (Constraint)
import Relude hiding (Constraint)

data TypeDefinition = TypeDefinition
  { constructors :: ![DataConstructor],
    name :: !Qualified,
    origin :: !Origin,
    parameters :: !([Parameter], [Constraint])
  }
  deriving (Eq, Ord, Show)
