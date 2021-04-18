{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Signature
-- Description : Type signatures
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Signature
  ( Signature (..),
    _Application,
    _Bottom,
    _Function,
    _Quantified,
    _Variable,
    _StackFunction,
    _Type,
    origin,
  )
where

import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (GeneralName)
import Mlatu.Origin (Origin)
import Mlatu.Type (Type)
import Mlatu.Type qualified as Type
import Mlatu.Uses (Uses (..))
import Optics.TH (makePrisms)
import Relude hiding (Constraint, Type)

-- | A parsed type signature.
data Signature
  = -- | @List\<T\>@
    Application Signature Signature Origin
  | -- | An empty stack.
    Bottom Origin Uses
  | -- | @A, B -> C, D +P +Q@
    Function [Signature] [Signature] Origin Uses
  | -- | @\<R..., T, +P\> (...)@
    Quantified [Parameter] Signature Origin
  | -- | @T@
    Variable GeneralName Origin Uses
  | -- | @R..., A, B -> S..., C, D +P +Q@
    StackFunction Signature [Signature] Signature [Signature] Origin Uses
  | -- | Produced when generating signatures for lifted quotations after
    -- typechecking.
    Type Type
  deriving (Show)

makePrisms ''Signature

-- | Signatures are compared regardless of origin.
instance Eq Signature where
  Application a b _ == Application c d _ = (a, b) == (c, d)
  Function a b _ c == Function d e _ f = (a, b, c) == (d, e, f)
  Quantified a b _ == Quantified c d _ = (a, b) == (c, d)
  Variable a _ b == Variable c _ d = (a, b) == (c, d)
  StackFunction a b c d _ e == StackFunction f g h i _ j =
    (a, b, c, d, e) == (f, g, h, i, j)
  _ == _ = False

deriving instance Ord Signature

origin :: Signature -> Origin
origin signature = case signature of
  Application _ _ o -> o
  Bottom o _ -> o
  Function _ _ o _ -> o
  Quantified _ _ o -> o
  Variable _ o _ -> o
  StackFunction _ _ _ _ o _ -> o
  Type t -> Type.origin t
