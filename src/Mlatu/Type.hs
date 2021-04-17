{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Mlatu.Type
-- Description : Types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Type
  ( Constructor (..),
    Type (..),
    TypeId (..),
    Var (..),
    pattern Bottom,
    pattern Fun,
    pattern Prod,
    pattern Sum,
    setOrigin,
    origin,
  )
where

import Mlatu.Kind (Kind (..))
import Mlatu.Name (Qualified (..), Unqualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Uses (Uses (..))
import Mlatu.Vocabulary qualified as Vocabulary
import Relude hiding (Sum, Type, void)

-- | This is the type language. It describes a system of conventional Hindley–
-- Milner types, with type constructors joined by type application, as well as
-- type variables and constants for constraint solving and instance checking,
-- respectively. It syntactically permits higher-ranked quantification, though
-- there are semantic restrictions on this, discussed in the presentation of the
-- inference algorithm. Type variables have explicit kinds.
data Type
  = (:@) Type Type
  | TypeConstructor Origin Uses Constructor
  | TypeVar Origin Uses Var
  | TypeConstant Origin Uses Var
  | Forall Origin Uses Var Type
  deriving (Ord, Show)

infixl 1 :@

pattern Bottom :: Origin -> Uses -> Type
pattern Bottom o u = TypeConstructor o u "Bottom"

pattern Fun :: Origin -> Uses -> Type -> Type -> Type
pattern Fun o u a b = TypeConstructor o u "Fun" :@ a :@ b

pattern Prod :: Origin -> Uses -> Type -> Type -> Type
pattern Prod o u a b = TypeConstructor o u "Prod" :@ a :@ b

pattern Sum :: Origin -> Uses -> Type -> Type -> Type
pattern Sum o u a b = TypeConstructor o u "Sum" :@ a :@ b

newtype Constructor = Constructor Qualified
  deriving (Ord, Eq, Hashable, Show)

data Var = Var Unqualified TypeId Kind
  deriving (Ord, Show)

instance Eq Var where
  -- We ignore the name hint for equality tests.
  Var _ a b == Var _ c d = (a, b) == (c, d)

origin :: Type -> Origin
origin = \case
  a :@ _ -> origin a
  TypeConstructor o _ _ -> o
  TypeVar o _ _ -> o
  TypeConstant o _ _ -> o
  Forall o _ _ _ -> o

setOrigin :: Origin -> Type -> Type
setOrigin o = go
  where
    go = \case
      a :@ b -> go a :@ go b
      TypeConstructor _ uses constructor -> TypeConstructor o uses constructor
      TypeVar _ uses var -> TypeVar o uses var
      TypeConstant _ uses var -> TypeConstant o uses var
      Forall _ uses var t -> Forall o uses var $ go t

-- | Type variables are distinguished by globally unique identifiers. This makes
-- it easier to support capture-avoiding substitution on types.
newtype TypeId = TypeId Int
  deriving (Enum, Bounded, Eq, Hashable, Ord, Show)

instance Eq Type where
  (a :@ b) == (c :@ d) = (a, b) == (c, d)
  TypeConstructor _ a b == TypeConstructor _ c d = (a, b) == (c, d)
  TypeVar _ a b == TypeVar _ c d = (a, b) == (c, d)
  TypeConstant _ a b == TypeConstant _ c d = (a, b) == (c, d)
  Forall _ a b c == Forall _ d e f = (a, b, c) == (d, e, f)
  _ == _ = False

instance Hashable Type where
  hashWithSalt s = \case
    a :@ b -> hashWithSalt s (0 :: Int, a, b)
    TypeConstructor _ a b -> hashWithSalt s (1 :: Int, a, b)
    TypeVar _ a b -> hashWithSalt s (2 :: Int, a, b)
    TypeConstant _ a b -> hashWithSalt s (3 :: Int, a, b)
    Forall _ a b c -> hashWithSalt s (4 :: Int, a, b, c)

instance Hashable Var where
  -- We ignore the name hint when hashing.
  hashWithSalt s (Var _ a b) = hashWithSalt s (0 :: Int, a, b)

instance IsString Constructor where
  fromString =
    Constructor
      . Qualified Vocabulary.global
      . Unqualified
      . toText
