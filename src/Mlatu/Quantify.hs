-- |
-- Module      : Mlatu.Quantify
-- Description : Quantifying generic terms
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Quantify
  ( term,
  )
where

import Mlatu.Kind qualified as Kind
import Mlatu.Term (Term (..))
import Mlatu.Type (Type (..), Var (..))

-- | Copies the top-level generic value-kinded type quantifiers from a polytype
-- to an expression, thereby making the expression generic, e.g.:
--
-- > dup, ∀α:ρ. ∀β:*. ∀γ:ε. (α × β → α × β × β) ε
-- >
-- > Λβ:*. dup
term :: Type -> Term a -> Term a
term (Forall origin (Var name x Kind.Value) t) e = Generic origin name x (term t e)
term (Forall _ _ t) e = term t e
term _ e = e
