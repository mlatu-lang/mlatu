{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Element
-- Description : Top-level program elements
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Element
  ( Element (..),
    _Trait,
    _Definition,
    _Metadata,
    _Term,
    _Data,
  )
where

import Mlatu.CodataDefinition (CodataDefinition)
import Mlatu.DataDefinition (DataDefinition)
import Mlatu.Definition (Definition)
import Mlatu.Metadata (Metadata)
import Mlatu.Term (Term)
import Mlatu.Trait (Trait)
import Optics.TH (makePrisms)

-- | A top-level program element.
data Element a
  = -- | @intrinsic@, @trait@
    Trait !Trait
  | -- | @define@, @instance@
    Definition !(Definition a)
  | -- | @about@
    Metadata !Metadata
  | -- | Top-level (@main@) code.
    Term !(Term a)
  | -- | @type@
    Data !DataDefinition
  | Codata !CodataDefinition

makePrisms ''Element
