-- |
-- Module      : Mlatu.Free
-- Description : Free variables of a type
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.TypeSystem.Free
  ( tvs,
    tvks,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Mlatu.Base.Kind (Kind)
import Mlatu.Base.Name (Unqualified)
import Mlatu.Base.Type (Type (..), TypeId, Var (..))
import Mlatu.TypeSystem.TypeEnv (TypeEnv)
import Mlatu.TypeSystem.Zonk qualified as Zonk

-- | Just the free variables of a type, without their kinds.
tvs :: TypeEnv -> Type -> Set TypeId
tvs tenv0 = Set.fromList . Map.keys . tvks tenv0

-- | Finds free variables (those not bound by any quantifier) and pures them
-- along with their kinds.
tvks :: TypeEnv -> Type -> Map TypeId (Unqualified, Kind)
tvks tenv x = go (Zonk.typ tenv x)
  where
    go :: Type -> Map TypeId (Unqualified, Kind)
    go (TypeVar _ (Var name i k)) = one (i, (name, k))
    go (Forall _ (Var _name i _) t') = Map.delete i $ go t'
    go (a :@ b) = Map.union (go a) (go b)
    go TypeConstructor {} = Map.empty
    go TypeConstant {} = Map.empty
    go TypeValue {} = Map.empty
