{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.TypeEnv
-- Description : Type inference environment
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.TypeEnv
  ( TypeEnv (..),
    empty,
    freshTv,
    freshTypeId,
    getClosed,
        tvs,
    vs,
    closure,
    sigs,
    currentType
  )
where

import Data.Map qualified as Map
import Mlatu.Kind (Kind)
import Mlatu.Monad (K)
import Mlatu.Name
  ( Closed (..),
    ClosureIndex (ClosureIndex),
    LocalIndex (LocalIndex),
    Qualified,
    Unqualified
  )
import Mlatu.Origin (Origin)
import Mlatu.Type (Type (..), TypeId (..), Var (..))
import Relude hiding (Type, empty)
import Relude.Extra (next)
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Optics.TH (makeLenses)
import Optics (view)

-- The typing environment tracks the state of inference. It answers the
-- following questions:
--
--  • What is the type of this type variable?
--  • What is the type of this local variable?
--  • What are the types of the current closure?
--  • What is the signature of this definition?
--
-- It also provides access to the state of globally unique ID generation.

data TypeEnv = TypeEnv
  { _tvs :: !(Map TypeId Type),
    _vs :: ![Type],
    _closure :: ![Type],
    _sigs :: !(Map Qualified Type),
    _currentType :: !(IORef TypeId)
  }

makeLenses ''TypeEnv

empty :: TypeEnv
empty =
  TypeEnv
    { _tvs = Map.empty,
      _vs = [],
      _closure = [],
      _sigs = Map.empty,
      _currentType = currentTypeId
    }

currentTypeId :: IORef TypeId
currentTypeId = unsafePerformIO (newIORef (TypeId 0))
{-# NOINLINE currentTypeId #-}

freshTv :: TypeEnv -> Unqualified -> Origin -> Kind -> K Type
freshTv tenv name origin k =
  TypeVar origin <$> (Var name <$> freshTypeId tenv <*> pure k)

freshTypeId :: TypeEnv -> K TypeId
freshTypeId tenv = do
  x <- liftIO $ readIORef $ view currentType tenv
  liftIO $ writeIORef (view currentType tenv) $ next x
  return x

instance Pretty TypeEnv where
  pPrint tenv =
    Pretty.vcat $
      map (\(v, t) -> Pretty.hsep [pPrint v, "~", pPrint t]) $
        Map.toList $ view tvs tenv

getClosed :: TypeEnv -> Closed -> Maybe Type
getClosed tenv name = case name of
  ClosedLocal (LocalIndex index) -> view vs tenv !!? index
  ClosedClosure (ClosureIndex index) -> view closure tenv !!? index
