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
  )
where

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
  { body :: !(Term a),
    category :: !Category,
    fixity :: !Fixity,
    inferSignature :: !Bool,
    merge :: !Merge,
    name :: !Qualified,
    origin :: !Origin,
    parent :: !(Maybe Parent),
    signature :: !Signature
  }
  deriving (Show)

instance Pretty (Definition a) where
  pPrint definition =
    Pretty.asDefinition
      (pPrint $ name definition)
      (pPrint $ signature definition)
      (pPrint $ body definition)
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
    { body = term,
      category = Category.Word,
      fixity = Operator.Postfix,
      inferSignature = True,
      merge = Merge.Compose,
      name = fromMaybe mainName mName,
      origin = o,
      parent = Nothing,
      signature =
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