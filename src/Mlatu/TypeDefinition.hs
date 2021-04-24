{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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
    constructors,
    name,
    origin,
    parameters,
    kind,
  )
where

import Mlatu.DataConstructor (DataConstructor)
import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Kind (Kind)
import Mlatu.Name (Qualified)
import Mlatu.Origin (Origin)
import Optics.TH (makeLenses)
import Relude hiding (Constraint)

data TypeDefinition = TypeDefinition
  { _constructors :: [DataConstructor],
    _name :: Qualified,
    _origin :: Origin,
    _parameters :: [Parameter],
    _kind :: Kind
  }
  deriving (Eq, Ord, Show)

makeLenses ''TypeDefinition
