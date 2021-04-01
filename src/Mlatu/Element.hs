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
    _Intrinsic,
    _Class,
    _WordDefinition,
    _Metadata,
    _Term,
    _TypeDefinition,
    _Instance,
  )
where

import Mlatu.Class (Class)
import Mlatu.Definition (WordDefinition)
import Mlatu.Instance (Instance)
import Mlatu.Intrinsic (Intrinsic)
import Mlatu.Metadata (Metadata)
import Mlatu.Term (Term)
import Mlatu.TypeDefinition (TypeDefinition)
import Optics.TH (makePrisms)

-- | A top-level program element.
data Element a
  = -- | @intrinsic@
    Intrinsic !Intrinsic
  | -- | @instance@
    Instance !(Instance a)
  | -- | @class@
    Class !Class
  | -- | @define@
    WordDefinition !(WordDefinition a)
  | -- | @about@
    Metadata !Metadata
  | -- | Top-level (@main@) code.
    Term !(Term a)
  | -- | @type@
    TypeDefinition !TypeDefinition

makePrisms ''Element
