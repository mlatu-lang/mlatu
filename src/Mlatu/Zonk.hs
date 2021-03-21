-- |
-- Module      : Mlatu.Zonk
-- Description : Fully substituting type variables
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Zonk
  ( typ,
    term,
  )
where

import Data.Map qualified as Map
import Mlatu.Term (Case (..), Else (..), Term (..), Value (..))
import Mlatu.Type (Type (..), Var (..))
import Mlatu.TypeEnv (TypeEnv)
import Mlatu.TypeEnv qualified as TypeEnv
import Relude hiding (Compose, Type)
import Optics

-- | Zonking a type fully substitutes all type variables. That is, if you have:
--
-- > t0 ~ t1
-- > t1 ~ Int64
--
-- Then zonking @t0@ gives you @Int64@.
typ :: TypeEnv -> Type -> Type
typ tenv0 = recur
  where
    recur t = case t of
      TypeConstructor {} -> t
      TypeValue {} -> error "TODO: zonk type value"
      TypeVar _origin (Var _name x _k) -> maybe t recur  (fmap .lookup x (view TypeEnv.tvs tenv0))
      TypeConstant {} -> t
      Forall origin var@(Var _ i _) t' ->
        Forall origin var $
          typ (over TypeEnv.tvs  (fmap .delete i) tenv0) t'
      a :@ b -> recur a :@ recur b

-- | Zonking a term zonks all the annotated types of its subterms. This could be
-- done more efficiently by sharing type references and updating them impurely,
-- but this implementation is easier to get right and understand.
term :: TypeEnv -> Term Type -> Term Type
term tenv0 = go
  where
    zonk = typ tenv0
    go t = case t of
      Coercion hint tref origin ->
        Coercion hint (zonk tref) origin
      Compose tref a b ->
        Compose (zonk tref) (go a) (go b)
      Generic name i a origin ->
        Generic name i (go a) origin
      Group a ->
        go a
      Lambda tref name varType body origin ->
        Lambda (zonk tref) name (zonk varType) (go body) origin
      Match hint tref cases else_ origin ->
        Match hint (zonk tref)  (fmap  goCase cases) (goElse else_) origin
        where
          goCase (Case name body caseOrigin) =
            Case name (go body) caseOrigin
          goElse (DefaultElse a b) = DefaultElse a b
          goElse (Else body elseOrigin) =
            Else (go body) elseOrigin
      New tref index size origin ->
        New (zonk tref) index size origin
      NewClosure tref index origin ->
        NewClosure (zonk tref) index origin
      NewVector tref size elemType origin ->
        NewVector (zonk tref) size (zonk elemType) origin
      Push tref value' origin ->
        Push (zonk tref) (value tenv0 value') origin
      Word tref fixity name params origin ->
        Word (zonk tref) fixity name params origin

value :: TypeEnv -> Value Type -> Value Type
value tenv0 = go
  where
    go v = case v of
      Capture names body -> Capture names $ term tenv0 body
      Character {} -> v
      Closed {} -> v
      Float {} -> v
      Integer {} -> v
      Local {} -> v
      Name {} -> v
      Quotation body -> Quotation $ term tenv0 body
      Text {} -> v
