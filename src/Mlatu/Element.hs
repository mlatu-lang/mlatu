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
_Declaration,
_Definition,
_Metadata,
_Synonym,
_Term,
_TypeDefinition
  )
where

import Mlatu.Declaration (Declaration)
import Mlatu.Definition (Definition)
import Mlatu.Metadata (Metadata)
import Mlatu.Synonym (Synonym)
import Mlatu.Term (Term)
import Mlatu.TypeDefinition (TypeDefinition)
import Optics (makePrisms)

-- | A top-level program element.
data Element a
  = -- | @intrinsic@, @trait@
    Declaration !Declaration
  | -- | @define@, @instance@
    Definition !(Definition a)
  | -- | @about@
    Metadata !Metadata
  | -- | @synonym@
    Synonym !Synonym
  | -- | Top-level (@main@) code.
    Term !(Term a)
  | -- | @type@
    TypeDefinition !TypeDefinition

makePrisms ''Element