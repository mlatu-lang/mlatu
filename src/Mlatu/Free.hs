-- |
-- Module      : Mlatu.Free
-- Description : Free variables of a type
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Free
  ( tvs,
    tvks,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Mlatu.Kind (Kind)
import Mlatu.Name (Unqualified)
import Mlatu.Type (Type (..), TypeId, Var (..))
import Mlatu.TypeEnv (TypeEnv)
import Mlatu.Zonk qualified as Zonk
import Relude hiding (Type)

-- | Just the free variables of a type, without their kinds.
tvs :: TypeEnv -> Type -> Set TypeId
tvs tenv0 = Set.fromList . Map.keys . tvks tenv0

-- | Finds free variables (those not bound by any quantifier) and pures them
-- along with their kinds.
tvks :: TypeEnv -> Type -> Map TypeId (Unqualified, Kind)
tvks tenv x = go (Zonk.typ tenv x)
  where
    go :: Type -> Map TypeId (Unqualified, Kind)
    go (TypeVar _ (Var name i k)) = Map.singleton i (name, k)
    go (Forall _ (Var _name i _) t') = Map.delete i $ go t'
    go (a :@ b) = Map.union (go a) (go b)
    go TypeConstructor {} = Map.empty
    go TypeConstant {} = Map.empty
