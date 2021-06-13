-- |
-- Module      : Mlatu.Zonk
-- Description : Fully substituting type variables
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.TypeSystem.Zonk
  ( typ,
    term,
  )
where

import Data.Map qualified as Map
import Mlatu.Base.Type (Type (..), Var (..))
import Mlatu.Front.Term (Term (..), Value (..))
import Mlatu.TypeSystem.TypeEnv (TypeEnv)
import Mlatu.TypeSystem.TypeEnv qualified as TypeEnv

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
      TypeVar _origin (Var _name x _k) -> maybe t recur (Map.lookup x (view TypeEnv.tvs tenv0))
      TypeConstant {} -> t
      Forall origin var@(Var _ i _) t' ->
        Forall origin var $
          typ (over TypeEnv.tvs (Map.delete i) tenv0) t'
      a :@ b -> recur a :@ recur b

-- | Zonking a term zonks all the annotated types of its subterms. This could be
-- done more efficiently by sharing type references and updating them impurely,
-- but this implementation is easier to get right and understand.
term :: TypeEnv -> Term Type -> Term Type
term tenv0 = go
  where
    zonk = typ tenv0
    go t = case t of
      Coercion hint tref origin -> Coercion hint (zonk tref) origin
      Compose tref a b -> Compose (zonk tref) (go a) (go b)
      Generic origin name i a -> Generic origin name i (go a)
      Group a -> go a
      Lambda origin tref name varType body ->
        Lambda origin (zonk tref) name (zonk varType) (go body)
      Match origin tref cases else_ ->
        Match origin (zonk tref) (goCase <$> cases) (goElse else_)
        where
          goCase = over _3 go
          goElse (o, Left a) = (o, Left a)
          goElse (o, Right body) = (o, Right (go body))
      New origin tref index size isNat -> New origin (zonk tref) index size isNat
      NewClosure origin tref index -> NewClosure origin (zonk tref) index
      Push origin tref value' -> Push origin (zonk tref) (value tenv0 value')
      Word origin tref name params -> Word origin (zonk tref) name params

value :: TypeEnv -> Value Type -> Value Type
value tenv0 = go
  where
    go v = case v of
      Capture names body -> Capture names $ term tenv0 body
      Character {} -> v
      Closed {} -> v
      Local {} -> v
      Name {} -> v
      Quotation body -> Quotation $ term tenv0 body
      Text {} -> v
