-- |
-- Module      : Mlatu.Signature
-- Description : Type signatures
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.Signature
  ( Signature (..),
    origin,
  )
where

import Mlatu.Base.Name (GeneralName)
import Mlatu.Base.Origin (Origin)
import Mlatu.Base.Type (Type)
import Mlatu.Base.Type qualified as Type
import Mlatu.Front.Parameter (Parameter)

-- | A parsed type signature.
data Signature
  = -- | @List\<T\>@
    Application !Signature !Signature !Origin
  | -- | An empty stack.
    Bottom !Origin
  | Grouped !Signature !Origin
  | -- | @A, B -> C, D +P +Q@
    Function ![Signature] ![Signature] ![GeneralName] !Origin
  | -- | @\<R..., T, +P\> (...)@
    Quantified ![Parameter] !Signature !Origin
  | -- | @T@
    Variable !GeneralName !Origin
  | -- | @R..., A, B -> S..., C, D +P +Q@
    StackFunction
      !Signature
      ![Signature]
      !Signature
      ![Signature]
      ![GeneralName]
      !Origin
  | -- | Produced when generating signatures for lifted quotations after
    -- typechecking.
    Type !Type
  deriving (Show)

-- | Signatures are compared regardless of origin.
instance Eq Signature where
  Application a b _ == Application c d _ = (a, b) == (c, d)
  Grouped a _ == Grouped b _ = a == b
  Function a b c _ == Function d e f _ = (a, b, c) == (d, e, f)
  Quantified a b _ == Quantified c d _ = (a, b) == (c, d)
  Variable a _ == Variable b _ = a == b
  StackFunction a b c d e _ == StackFunction f g h i j _ =
    (a, b, c, d, e) == (f, g, h, i, j)
  _ == _ = False

deriving instance Ord Signature

origin :: Signature -> Origin
origin signature = case signature of
  Application _ _ o -> o
  Grouped _ o -> o
  Bottom o -> o
  Function _ _ _ o -> o
  Quantified _ _ o -> o
  Variable _ o -> o
  StackFunction _ _ _ _ _ o -> o
  Type t -> Type.origin t
