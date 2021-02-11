module Test.Common
  ( Sign (..),
    ioPermission,
  )
where

import Mlatu.Name
  ( GeneralName (QualifiedName),
    Qualified (Qualified),
  )
import Mlatu.Vocabulary qualified as Vocabulary
import Relude

data Sign = Negative | Positive
  deriving (Eq, Ord, Show)

ioPermission :: [GeneralName]
ioPermission = [QualifiedName $ Qualified Vocabulary.global "IO"]
