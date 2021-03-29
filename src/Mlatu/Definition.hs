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
module Mlatu.Definition
  ( WordDefinition (..),
    ConstructorDefinition (..),
    PermissionDefinition (..),
    constructorBody,
    constructorName,
    constructorOrigin,
    constructorSignature,
    constructorParent,
    main,
    mainName,
    wordName,
    wordBody,
    wordFixity,
    wordInferSignature,
    wordMerge,
    wordOrigin,
    wordSignature,
    permissionName,
    permissionBody,
    permissionFixity,
    permissionSignature,
    permissionOrigin,
  )
where

import Mlatu.Entry.Merge (Merge)
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Kind (Kind (..))
import Mlatu.Name (ConstructorIndex, GeneralName (..), Qualified (..))
import Mlatu.Operator (Fixity)
import Mlatu.Operator qualified as Operator
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Term)
import Mlatu.Term qualified as Term
import Mlatu.Vocabulary qualified as Vocabulary
import Optics
import Relude hiding (Type)

data WordDefinition a = WordDefinition
  { _wordName :: !Qualified,
    _wordBody :: !(Term a),
    _wordFixity :: !Fixity,
    _wordInferSignature :: !Bool,
    _wordMerge :: !Merge,
    _wordOrigin :: !Origin,
    _wordSignature :: !Signature
  }
  deriving (Ord, Eq, Show)

makeLenses ''WordDefinition

data PermissionDefinition a = PermissionDefinition
  { _permissionName :: !Qualified,
    _permissionBody :: !(Term a),
    _permissionFixity :: !Fixity,
    _permissionOrigin :: !Origin,
    _permissionSignature :: !Signature
  }
  deriving (Ord, Eq, Show)

makeLenses ''PermissionDefinition

data ConstructorDefinition a = ConstructorDefinition
  { _constructorName :: !Qualified,
    _constructorBody :: !(ConstructorIndex, Int),
    _constructorOrigin :: !Origin,
    _constructorSignature :: !Signature,
    _constructorParent :: !Qualified
  }
  deriving (Ord, Eq, Show)

makeLenses ''ConstructorDefinition

-- | The main definition, created implicitly from top-level code in program
-- fragments.
main ::
  -- | List of permissions implicitly granted.
  [GeneralName] ->
  -- | Override default name.
  Maybe Qualified ->
  -- | Body.
  Term a ->
  WordDefinition a
main permissions mName term =
  WordDefinition
    { _wordBody = term,
      _wordFixity = Operator.Postfix,
      _wordInferSignature = True,
      _wordMerge = Merge.Compose,
      _wordName = fromMaybe mainName mName,
      _wordOrigin = o,
      _wordSignature =
        Signature.Quantified
          [Parameter o "R" Stack Nothing]
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
mainName = Qualified Vocabulary.global "main"
