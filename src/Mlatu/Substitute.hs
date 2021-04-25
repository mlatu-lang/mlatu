-- |
-- Module      : Mlatu.Substitute
-- Description : Substituting type variables
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Substitute
  ( term,
    typ,
  )
where

import Data.Set qualified as Set
import Mlatu.Free qualified as Free
import Mlatu.Kind qualified as Kind
import Mlatu.Monad (M)
import Mlatu.Term (Case (..), Else (..), Term (..))
import Mlatu.Type (Type (..), TypeId, Var (..))
import Mlatu.TypeEnv (TypeEnv, freshTypeId)
import Relude hiding (Compose, Type)

-- | Capture-avoiding substitution of a type variable α with a type τ throughout
-- a type σ, [α ↦ τ]σ.
typ :: TypeEnv -> TypeId -> Type -> Type -> M Type
typ tenv0 x a = recur
  where
    recur t = case t of
      Forall origin var@(Var name x' k) t'
        | x == x' -> pure t
        | x' `Set.notMember` Free.tvs tenv0 t' -> Forall origin var <$> recur t'
        | otherwise -> do
          z <- freshTypeId tenv0
          t'' <- typ tenv0 x' (TypeVar origin $ Var name z k) t'
          Forall origin (Var name z k) <$> recur t''
      TypeVar _ (Var _name x' _) | x == x' -> pure a
      m :@ n -> (:@) <$> recur m <*> recur n
      _noSubst -> pure t

term :: TypeEnv -> TypeId -> Type -> Term Type -> M (Term Type)
term tenv x a = recur
  where
    recur t = case t of
      Coercion hint tref origin -> Coercion hint <$> go tref <*> pure origin
      Compose tref t1 t2 -> Compose <$> go tref <*> recur t1 <*> recur t2
      Generic name x' body origin -> do
        -- FIXME: Generics could eventually quantify over non-value kinds.
        let k = Kind.Star
        z <- freshTypeId tenv
        body' <- term tenv x' (TypeVar origin $ Var name z k) body
        Generic name z <$> recur body' <*> pure origin
      Group body -> recur body
      Lambda tref name varType body s origin ->
        Lambda <$> go tref
          <*> pure name
          <*> go varType
          <*> recur body
          <*> pure s
          <*> pure origin
      Match hint tref cases else_ origin ->
        Match hint <$> go tref
          <*> traverse goCase cases
          <*> goElse else_
          <*> pure origin
        where
          goCase :: Case Type -> M (Case Type)
          goCase (Case name body caseOrigin) =
            Case name <$> recur body <*> pure caseOrigin

          goElse :: Else Type -> M (Else Type)
          goElse (DefaultElse elseType elseOrigin) = pure $ DefaultElse elseType elseOrigin
          goElse (Else body elseOrigin) = Else <$> recur body <*> pure elseOrigin
      New tref index size origin ->
        New
          <$> go tref <*> pure index <*> pure size <*> pure origin
      NewClosure tref size origin ->
        NewClosure <$> go tref
          <*> pure size
          <*> pure origin
      NewVector tref size elemType origin ->
        NewVector <$> go tref
          <*> pure size
          <*> go elemType
          <*> pure origin
      Push tref value origin -> Push <$> go tref <*> pure value <*> pure origin
      Word tref name args origin ->
        Word <$> go tref
          <*> pure name
          <*> traverse go args
          <*> pure origin
    {-# INLINEABLE recur #-}

    go = typ tenv x a
{-# INLINEABLE term #-}
