-- |
-- Module      : Mlatu.Unify
-- Description : Unification of types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Unify
  ( function,
    typ,
  )
where

import Data.Map qualified as Map
import Mlatu.Informer (Informer (..))
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Kind (Kind (..))
import Mlatu.Monad (M)
import Mlatu.Occurrences (occurs)
import Mlatu.Origin (Origin)
import Mlatu.Report qualified as Report
import Mlatu.Type (Type (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.TypeEnv (TypeEnv, freshTv)
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Zonk qualified as Zonk
import Optics
import Relude hiding (Type)

-- | There are two kinds of unification going on here: basic logical unification
-- for value types, and row unification for permission types.
typ :: TypeEnv -> Type -> Type -> M TypeEnv
typ tenv0 t1 t2 = case (t1', t2') of
  _ | t1' == t2' -> pure tenv0
  (TypeVar origin x, t) -> unifyTv tenv0 origin x t
  (_, TypeVar {}) -> commute
  -- FIXME: Unify the kinds here?
  (a, Forall origin (Var name x k) t) -> do
    (b, _, tenv1) <- Instantiate.typ tenv0 origin name x k t
    typ tenv1 a b
  (Forall {}, _) -> commute
  -- We fall back to regular unification for value type constructors. This makes
  -- the somewhat iffy assumption that there is no higher-kinded polymorphism
  -- going on between value type constructors and permission type constructors.

  (a :@ b, c :@ d) -> do
    tenv1 <- typ tenv0 a c
    typ tenv1 b d
  _mismatch -> do
    report $ Report.makeError $ Report.TypeMismatch t1' t2'
    halt

    -- Unification is commutative. If we fail to handle a case, this can result in
    -- an infinite loop.
  where
    t1' = Zonk.typ tenv0 t1
    t2' = Zonk.typ tenv0 t2
    commute = typ tenv0 t2 t1

-- Unification of a type variable with a type simply looks up the current value
-- of the variable and unifies it with the type; if the variable does not exist,
-- it is added to the environment and unified with the type.
--
-- The only interesting bits here are the occurs check, which prevents
-- constructing infinite types, and the condition that prevents declaring a
-- variable as equal to itself. Without both of these, zonking could fail to
-- terminate.
--
-- See: Occurs Checks

unifyTv :: TypeEnv -> Origin -> Var -> Type -> M TypeEnv
unifyTv tenv0 origin v@(Var _name x _) t = case t of
  TypeVar _origin (Var _name y _) | x == y -> pure tenv0
  TypeVar {} -> declare
  _nonVar ->
    if occurs tenv0 x (Zonk.typ tenv0 t)
      then
        let t' = Zonk.typ tenv0 t
         in do
              report $
                Report.makeError $
                  Report.Chain $
                    [ Report.TypeMismatch (TypeVar origin v) t',
                      Report.OccursCheckFailure (TypeVar origin v) t'
                    ]
                      ++ case t' of
                        Type.Prod {} -> [Report.StackDepthMismatch (Type.origin t')]
                        _nonProd -> []

              halt
      else declare
  where
    declare = pure $ over TypeEnv.tvs (Map.insert x t) tenv0

-- | A convenience function for unifying a type with a function type.
function :: TypeEnv -> Type -> M (Type, Type, TypeEnv)
function tenv0 t = case t of
  Type.Fun _ a b -> pure (a, b, tenv0)
  _nonFun -> do
    let origin = Type.origin t
    a <- freshTv tenv0 "A" origin Stack
    b <- freshTv tenv0 "B" origin Stack
    tenv1 <- typ tenv0 t $ Type.Fun origin a b
    pure (a, b, tenv1)
