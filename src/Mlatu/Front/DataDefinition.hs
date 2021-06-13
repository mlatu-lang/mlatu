{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.TypeDefinition
-- Description : Definitions of types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.DataDefinition
  ( DataDefinition (..),
    constructors,
    name,
    origin,
    parameters,
  )
where

import Mlatu.Base.Name (Qualified, Unqualified)
import Mlatu.Base.Origin (Origin)
import Mlatu.Front.Parameter (Parameter)
import Mlatu.Front.Signature (Signature)

data DataDefinition = DataDefinition
  { _constructors :: ![(Unqualified, [Signature], [Signature], Origin)],
    _name :: !Qualified,
    _origin :: !Origin,
    _parameters :: ![Parameter]
  }
  deriving (Eq, Ord, Show)

makeLenses ''DataDefinition
