-- |
-- Module      : Mlatu.Regeneralize
-- Description : Stack-generalization of types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.TypeSystem.Regeneralize
  ( regeneralize,
  )
where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.List (deleteBy)
import Data.Map qualified as Map
import Mlatu.Base.Kind (Kind)
import Mlatu.Base.Name (Unqualified)
import Mlatu.Base.Type (Type (..), TypeId, Var (..))
import Mlatu.Base.Type qualified as Type
import Mlatu.TypeSystem.Free qualified as Free
import Mlatu.TypeSystem.Occurrences (occurrences)
import Mlatu.TypeSystem.TypeEnv (TypeEnv)

-- | Because all functions are polymorphic with respect to the part of the stack
-- they don't touch, all words of order n can be regeneralized to words of rank
-- n with respect to the stack-kinded type variables.
--
-- This means that if a stack-kinded (ρ) type variable occurs only twice in a
-- type, in the bottommost position on both sides of a function arrow, then its
-- scope can be reduced to only that function arrow by introducing a
-- higher-ranked quantifier. This is a more conservative rule than used in
-- \"Simple type inference for higher-order stack languages\". For example, the
-- type of @map@:
--
-- > fmap :: ∀ρσαβ. ρ × List α × (σ × α → σ × β) → ρ × List β
--
-- Can be regeneralized like so:
--
-- > fmap :: ∀ραβ. ρ × List α × (∀σ. σ × α → σ × β) → ρ × List β
--
-- In order to correctly regeneralize a type, it needs to contain no
-- higher-ranked quantifiers.
regeneralize :: TypeEnv -> Type -> Type
regeneralize tenv t =
  let (t', vars) = runWriter $ go t
   in foldr addForall t' $
        foldr
          (deleteBy ((==) `on` fst))
          (Map.toList (Free.tvks tenv t'))
          vars
  where
    addForall :: (TypeId, (Unqualified, Kind)) -> Type -> Type
    addForall (i, (name, k)) = Forall (Type.origin t) (Var name i k)

    go :: Type -> Writer [(TypeId, (Unqualified, Kind))] Type
    go t' = case t' of
      Type.Fun _ a b e
        | TypeVar origin (Var name c k) <- bottommost a,
          TypeVar _ (Var _name d _) <- bottommost b,
          c == d ->
          do
            when (occurrences tenv c t == 2) $ tell [(c, (name, k))]
            a' <- go a
            b' <- go b
            e' <- go e
            pure $ Forall origin (Var name c k) $ Type.Fun origin a' b' e'
      Type.Prod o a b -> do
        a' <- go a
        b' <- go b
        pure $ Type.Prod o a' b'
      -- FIXME: This should descend into the quantified type.
      Forall {} -> pure t'
      a :@ b -> (:@) <$> go a <*> go b
      _alreadyGeneralized -> pure t'

bottommost :: Type -> Type
bottommost (Type.Prod _ a _) = bottommost a
bottommost a = a
