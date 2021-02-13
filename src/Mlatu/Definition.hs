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
    isMain,
    main,
    mainName,
    body,
    category,
    fixity,
    inferSignature,
    merge,
    name,
    origin,
    parent,
    signature,
  )
where


import Control.Lens (makeLenses, (^.))
import Mlatu.Entry.Category (Category)
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Merge (Merge)
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Entry.Parent (Parent)
import Mlatu.Kind (Kind (..))
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Operator (Fixity)
import Mlatu.Operator qualified as Operator
import Mlatu.Origin (Origin)
import Mlatu.Pretty qualified as Pretty
import Mlatu.Signature (Signature)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Term)
import Mlatu.Term qualified as Term
import Mlatu.Token qualified as Token
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data Definition a = Definition
  { _body :: !(Term a),
    _category :: !Category,
    _fixity :: !Fixity,
    _inferSignature :: !Bool,
    _merge :: !Merge,
    _name :: !Qualified,
    _origin :: !Origin,
    _parent :: !(Maybe Parent),
    _signature :: !Signature
  }
  deriving (Show)

makeLenses ''Definition

instance Pretty (Definition a) where
  pPrint definition =
    Pretty.asDefinition
      (pPrint $ definition ^. name)
      (pPrint $ definition ^. signature)
      (pPrint $ definition ^. body)
      (pPrint Token.Define)

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
      _fixity = Operator.Postfix,
      _inferSignature = True,
      _merge = Merge.Compose,
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
mainName = Qualified Vocabulary.global "main"

-- | Whether a given definition refers to (the default-named) @main@.
isMain :: Definition a -> Bool
isMain def = def ^. name == mainName
