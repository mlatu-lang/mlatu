{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Entry
-- Description : Dictionary entries
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Entry
  ( Entry (..),
    _Word,
    _Metadata,
    _Trait,
    _Type,
    _InstantiatedType,
  )
where

import Mlatu.DataConstructor (DataConstructor)
import Mlatu.Entry.Category (Category)
import Mlatu.Entry.Merge (Merge)
import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Entry.Parent (Parent)
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Mlatu.Term (Term)
import Mlatu.Type (Type)
import Optics.TH (makePrisms)
import Relude hiding (Constraint, Type)

-- | An entry in the dictionary.
--
-- FIXME: This could use significant cleaning up. We could possibly make each
-- constructor into a separate 'Map' in the 'Dictionary'.
data Entry
  = -- | A word definition. If the implementation is 'Nothing', this is a
    -- declaration: it can be used for type checking and name resolution, but not
    -- compilation. If the parent is a trait, this is a trait instance, with
    -- instance mangling. If the parent is a type, this is a constructor.
    -- Definitions without signatures are disallowed by the surface syntax, but
    -- they are generated for lifted lambdas, as those have already been
    -- typechecked by the time quotations are flattened into top-level definitions
    -- ("Mlatu.Desugar.Quotations").
    Word
      !Category
      !Merge
      !Origin
      !(Maybe Parent)
      !(Maybe Signature)
      !(Maybe (Term Type))
  | -- | Untyped metadata from @about@ blocks. Used internally for operator
    -- precedence and associativity.
    Metadata !Origin !(Term ())
  | -- | A trait to which other entries can link.
    Trait !Origin !Signature
  | -- | A data type with some generic parameters.
    Type !Origin ![Parameter] ![DataConstructor]
  | -- | An instantiation of a data type, with the given size.
    InstantiatedType !Origin !Int
  deriving (Show)

makePrisms ''Entry
