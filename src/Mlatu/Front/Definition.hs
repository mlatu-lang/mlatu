{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Definition
-- Description : Definitions of words, instances, and permissions
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.Definition
  ( Definition (..),
    Merge (..),
    Parent (..),
    Category (..),
    main,
    mainName,
    category,
    name,
    body,
    inferSignature,
    merge,
    origin,
    signature,
    parent,
  )
where

import Mlatu.Base.Kind (Kind (..))
import Mlatu.Base.Name (GeneralName (..), Qualified (..))
import Mlatu.Base.Origin (Origin)
import Mlatu.Base.Vocabulary
import Mlatu.Front.Parameter (Parameter (..))
import Mlatu.Front.Signature (Signature)
import Mlatu.Front.Signature qualified as Signature
import Mlatu.Front.Term (Term)
import Mlatu.Front.Term qualified as Term

data Merge = DenyMerge | ComposeMerge
  deriving (Ord, Eq, Show)

data Parent
  = TraitParent !Qualified
  | TypeParent !Qualified
  | RecordParent !Qualified
  deriving (Ord, Eq, Show)

data Category
  = ConstructorWord
  | InstanceWord
  | PermissionWord
  | DefinedWord
  | DeconstructorWord
  | ExternWord
  deriving (Ord, Eq, Show)

data Definition a = Definition
  { _category :: !Category,
    _name :: !Qualified,
    _body :: !(Term a),
    _inferSignature :: !Bool,
    _merge :: !Merge,
    _origin :: !Origin,
    _signature :: !Signature,
    _parent :: !(Maybe Parent)
  }
  deriving (Ord, Eq, Show)

makeLenses ''Definition

-- | The main definition, created implicitly from top-level code in program
-- fragments.
main ::
  -- | List of permissions implicitly granted.
  [GeneralName] ->
  -- | Override default name.
  Maybe Qualified ->
  -- | Body.
  Term a ->
  Definition a
main permissions mName term =
  Definition
    { _body = term,
      _category = DefinedWord,
      _inferSignature = True,
      _merge = ComposeMerge,
      _name = fromMaybe mainName mName,
      _origin = o,
      _parent = Nothing,
      _signature =
        Signature.Quantified
          [Parameter o "R" Stack]
          ( Signature.StackFunction
              (Signature.Variable "R" o)
              []
              (Signature.Variable "R" o)
              []
              permissions
              o
          )
          o
    }
  where
    o = Term.origin term

-- | Default name of main definition.
mainName :: Qualified
mainName = Global "main"
