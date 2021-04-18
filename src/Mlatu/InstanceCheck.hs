-- |
-- Module      : Mlatu.InstanceCheck
-- Description : Checking types against signatures
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.InstanceCheck
  ( instanceCheck,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Mlatu.Free qualified as Free
import Mlatu.Informer (Informer (..))
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Monad (M, attempt)
import Mlatu.Origin (Origin)
import Mlatu.Report qualified as Report
import Mlatu.Substitute qualified as Substitute
import Mlatu.Type (Constructor (..), Type (..), TypeId, Var (..))
import Mlatu.Type qualified as Type
import Mlatu.TypeEnv (TypeEnv, freshTypeId)
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Unify qualified as Unify
import Mlatu.Zonk qualified as Zonk
import Optics
import Prettyprinter (Doc)
import Relude hiding (Type)

-- | Checks whether one type is a generic instance of another, used for checking
-- type signatures. Remember, when using this function, which way the subtyping
-- relation goes: @∀α. α → α@ is a generic instance of @int → int@, not the
-- other way around!
instanceCheck :: Doc a -> Type -> Doc a -> Type -> M ()
instanceCheck _ aScheme _ bScheme = do
  let tenv0 = TypeEnv.empty
  let aType = aScheme
  (ids, bType) <- skolemize tenv0 bScheme
  let envTypes = Map.elems (view TypeEnv.tvs tenv0)
  success <- attempt $ subsumptionCheck tenv0 aType bType
  unless success failure
  let escaped = Set.unions $ Free.tvs tenv0 <$> aScheme : bScheme : envTypes
  -- Free.tvs tenv0 aScheme `Set.union` Free.tvs tenv0 bScheme
  let bad = Set.filter (`Set.member` escaped) ids
  unless (Set.null bad) failure
  where
    failure = report $ Report.makeError $ Report.FailedInstanceCheck aScheme bScheme

-- | Skolemization replaces each quantified type variable with a type constant
-- that unifies only with itself.
skolemize :: TypeEnv -> Type -> M (Set TypeId, Type)
skolemize tenv0 t = case t of
  Forall origin uses (Var name x k) t' -> do
    c <- freshTypeId tenv0
    substituted <-
      Substitute.typ
        tenv0
        x
        (TypeConstant origin uses $ Var name c k)
        t'
    (c', t'') <- skolemize tenv0 substituted
    pure (Set.insert c c', t'')
  -- TForall _ t' -> skolemize tenv0 t'
  Type.Fun origin uses a b -> do
    (ids, b') <- skolemize tenv0 b
    pure (ids, Type.Fun origin uses a b')
  _nonQuantified -> pure (Set.empty, t)

-- | Subsumption checking is largely the same as unification, accounting for
-- function type variance: if @(a -> b) <: (c -> d)@ then @b <: d@ (covariant)
-- but @c <: a@ (contravariant).
subsumptionCheck :: TypeEnv -> Type -> Type -> M TypeEnv
subsumptionCheck tenv0 (Forall origin uses (Var name x k) t) t2 = do
  (t1, _, tenv1) <- Instantiate.typ tenv0 origin uses name x k t
  subsumptionCheck tenv1 t1 t2
subsumptionCheck tenv0 t1 (Type.Fun _ _uses a' b') = do
  (_, a, b, tenv1) <- Unify.function tenv0 t1
  subsumptionCheckFun tenv1 a b a' b'
subsumptionCheck tenv0 (Type.Fun _ _uses a b) t2 = do
  (_, a', b', tenv1) <- Unify.function tenv0 t2
  subsumptionCheckFun tenv1 a b a' b'
subsumptionCheck tenv0 t1 t2 = Unify.typ tenv0 t1 t2

subsumptionCheckFun ::
  TypeEnv -> Type -> Type -> Type -> Type -> M TypeEnv
subsumptionCheckFun tenv0 a b a' b' = do
  tenv1 <- subsumptionCheck tenv0 a' a
  subsumptionCheck tenv1 b b'
