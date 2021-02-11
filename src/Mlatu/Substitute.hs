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
import Mlatu.Monad (K)
import Mlatu.Term (Case (..), Else (..), Term (..))
import Mlatu.Type (Type (..), TypeId, Var (..))
import Mlatu.TypeEnv (TypeEnv, freshTypeId)
import Relude hiding (Compose, Type)

-- | Capture-avoiding substitution of a type variable α with a type τ throughout
-- a type σ, [α ↦ τ]σ.
typ :: TypeEnv -> TypeId -> Type -> Type -> K Type
typ tenv0 x a = recur
  where
    recur t = case t of
      Forall origin var@(Var name x' k) t'
        | x == x' -> return t
        | x' `Set.notMember` Free.tvs tenv0 t' -> Forall origin var <$> recur t'
        | otherwise -> do
          z <- freshTypeId tenv0
          t'' <- typ tenv0 x' (TypeVar origin $ Var name z k) t'
          Forall origin (Var name z k) <$> recur t''
      TypeVar _ (Var _name x' _) | x == x' -> return a
      m :@ n -> (:@) <$> recur m <*> recur n
      _noSubst -> return t

term :: TypeEnv -> TypeId -> Type -> Term Type -> K (Term Type)
term tenv x a = recur
  where
    recur t = case t of
      Coercion hint tref origin -> Coercion hint <$> go tref <*> pure origin
      Compose tref t1 t2 -> Compose <$> go tref <*> recur t1 <*> recur t2
      Generic name x' body origin -> do
        -- FIXME: Generics could eventually quantify over non-value kinds.
        let k = Kind.Value
        z <- freshTypeId tenv
        body' <- term tenv x' (TypeVar origin $ Var name z k) body
        Generic name z <$> recur body' <*> pure origin
      Group body -> recur body
      Lambda tref name varType body origin ->
        Lambda <$> go tref
          <*> pure name
          <*> go varType
          <*> recur body
          <*> pure origin
      Match hint tref cases else_ origin ->
        Match hint <$> go tref
          <*> mapM goCase cases
          <*> goElse else_
          <*> pure origin
        where
          goCase :: Case Type -> K (Case Type)
          goCase (Case name body caseOrigin) =
            Case name <$> recur body <*> pure caseOrigin

          goElse :: Else Type -> K (Else Type)
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
      Word tref fixity name args origin ->
        Word <$> go tref
          <*> pure fixity
          <*> pure name
          <*> mapM go args
          <*> pure origin

    go = typ tenv x a
