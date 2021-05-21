{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.TypeDefinition
-- Description : Definitions of types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.DataDefinition
  ( DataDefinition (..),
    constructors,
    name,
    origin,
    parameters,
  )
where

import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (Qualified, Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Optics.TH (makeLenses)
import Relude hiding (Constraint)

data DataDefinition = DataDefinition
  { _constructors :: ![(Unqualified, [Signature], Origin)],
    _name :: !Qualified,
    _origin :: !Origin,
    _parameters :: ![Parameter]
  }
  deriving (Eq, Ord, Show)

makeLenses ''DataDefinition
