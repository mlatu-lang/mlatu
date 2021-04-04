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
    constructorBody,
    constructorName,
    constructorOrigin,
    constructorSignature,
    constructorParent,
    main,
    mainName,
    wordName,
    wordBody,
    wordInferSignature,
    wordMerge,
    wordOrigin,
    wordSignature,
  )
where

import Mlatu.Entry.Merge (Merge)
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Kind (Kind (..))
import Mlatu.Name (ConstructorIndex, Qualified (..))
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
    _wordInferSignature :: !Bool,
    _wordMerge :: !Merge,
    _wordOrigin :: !Origin,
    _wordSignature :: !Signature
  }
  deriving (Ord, Eq, Show)

makeLenses ''WordDefinition

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
  -- | Override default name.
  Maybe Qualified ->
  -- | Body.
  Term a ->
  WordDefinition a
main mName term =
  WordDefinition
    { _wordBody = term,
      _wordInferSignature = True,
      _wordMerge = Merge.Compose,
      _wordName = fromMaybe mainName mName,
      _wordOrigin = o,
      _wordSignature =
        Signature.Quantified
          [Parameter o "R" Stack Nothing]
          ( Signature.Function
              [Signature.Variable "R" o]
              [Signature.Variable "R" o]
              o
          )
          o
    }
  where
    o = Term.origin term

-- | Default name of main definition.
mainName :: Qualified
mainName = Qualified Vocabulary.global "main"
