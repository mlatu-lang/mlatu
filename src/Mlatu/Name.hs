-- |
-- Module      : Mlatu.Name
-- Description : Program identifiers
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Name
  ( GeneralName (..),
    Closed (..),
    ClosureIndex (..),
    ConstructorIndex (..),
    LocalIndex (..),
    Qualified (..),
    Qualifier (..),
    Root (..),
    Unqualified (..),
    isOperatorName,
    toParts,
    qualifierFromName,
  )
where

import Data.Char (isLetter)
import Data.Text qualified as Text
import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | A dynamic name, which might be 'Qualified', 'Unqualified', or local.
data GeneralName
  = QualifiedName !Qualified
  | UnqualifiedName !Unqualified
  | LocalName !LocalIndex
  deriving (Eq, Ord, Show)

instance IsString GeneralName where
  fromString = UnqualifiedName . fromString

-- | A qualified name is an unqualified name (@x@) plus a qualifier (@q::@).
data Qualified = Qualified
  { qualifierName :: !Qualifier,
    unqualifiedName :: !Unqualified
  }
  deriving (Eq, Ord, Show)

-- | A qualifier is a list of vocabulary names, rooted globally or within the
-- current vocabulary.
data Qualifier = Qualifier !Root ![Text]
  deriving (Eq, Ord, Show)

-- | A 'Relative' qualifier refers to a sub-vocabulary of the current one. An
-- 'Absolute' qualifier refers to the global vocabulary.
data Root = Relative | Absolute
  deriving (Eq, Ord, Show)

-- | An unqualified name is an ordinary symbol.
newtype Unqualified = Unqualified Text
  deriving (Eq, Ord, Show)

-- | A closed name is a local or closure variable that was captured by a
-- quotation. FIXME: this can be removed if closure variables are rewritten into
-- implicit locals.
data Closed
  = ClosedLocal !LocalIndex
  | ClosedClosure !ClosureIndex
  deriving (Ord, Eq, Show)

-- | An index into a closure.
newtype ClosureIndex = ClosureIndex Int
  deriving (Eq, Ord, Show)

-- | The index of a data type constructor.
newtype ConstructorIndex = ConstructorIndex Int
  deriving (Eq, Ord, Show)

-- | The De Bruijn index of a local variable.
newtype LocalIndex = LocalIndex Int
  deriving (Eq, Ord, Show)

-- TODO: Use types, not strings.
isOperatorName :: Qualified -> Bool
isOperatorName = match . unqualifiedName
  where
    match (Unqualified name) =
      not $
        liftA2 (||) (Text.all isLetter) (== "_") $
          Text.take 1 name

toParts :: Qualified -> [Text]
toParts (Qualified (Qualifier _root parts) (Unqualified part)) =
  parts ++ [part]

qualifierFromName :: Qualified -> Qualifier
qualifierFromName (Qualified (Qualifier root parts) (Unqualified name)) =
  Qualifier root (parts ++ [name])

instance Hashable Qualified where
  hashWithSalt s (Qualified qualifier unqualified) =
    hashWithSalt s (0 :: Int, qualifier, unqualified)

instance Hashable Qualifier where
  hashWithSalt s (Qualifier root parts) =
    hashWithSalt s (0 :: Int, root, Text.concat parts)

instance Hashable Root where
  hashWithSalt s Relative = hashWithSalt s (0 :: Int)
  hashWithSalt s Absolute = hashWithSalt s (1 :: Int)

instance Hashable Unqualified where
  hashWithSalt s (Unqualified name) = hashWithSalt s (0 :: Int, name)

instance IsString Unqualified where
  fromString = Unqualified . toText

instance Pretty Qualified where
  pPrint (Qualified (Qualifier Absolute []) unqualifiedName) = pPrint unqualifiedName
  pPrint (Qualified qualifier unqualifiedName) =
    pPrint qualifier
      Pretty.<> "::"
      Pretty.<> pPrint unqualifiedName

instance Pretty Qualifier where
  pPrint (Qualifier Absolute parts) = pPrint $ Qualifier Relative ("_" : parts)
  pPrint (Qualifier Relative parts) =
    Pretty.text $
      toString $ Text.intercalate "::" parts

instance Pretty Unqualified where
  pPrint (Unqualified unqualified) = Pretty.text $ toString unqualified

instance Pretty GeneralName where
  pPrint (QualifiedName qualified) = pPrint qualified
  pPrint (UnqualifiedName unqualified) = pPrint unqualified
  pPrint (LocalName index) = pPrint index

instance Pretty LocalIndex where
  pPrint (LocalIndex i) = "local." Pretty.<> Pretty.int i

instance Pretty ClosureIndex where
  pPrint (ClosureIndex i) = "closure." Pretty.<> Pretty.int i

instance Pretty Closed where
  pPrint (ClosedLocal index) = pPrint index
  pPrint (ClosedClosure (ClosureIndex index)) = pPrint index
