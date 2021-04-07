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
  ( Definition (..),
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

import Mlatu.Entry.Category (Category)
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Merge (Merge)
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Entry.Parent (Parent (..))
import Mlatu.Kind (Kind (..))
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Term)
import Mlatu.Term qualified as Term
import Mlatu.Vocabulary qualified as Vocabulary
import Optics
import Relude

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
      _category = Category.Word,
      _inferSignature = True,
      _merge = Merge.Compose,
      _name = fromMaybe mainName mName,
      _origin = o,
      _parent = Nothing,
      _signature =
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
