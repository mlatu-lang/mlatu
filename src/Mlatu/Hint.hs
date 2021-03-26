module Mlatu.Hint (fragment) where

import Mlatu.Declaration qualified as Declaration
import Mlatu.Definition qualified as Definition
import Mlatu.Fragment qualified as Fragment
import Mlatu.Informer (Informer (..))
import Mlatu.Metadata qualified as Metadata
import Mlatu.Monad (M)
import Mlatu.Name (Qualified (..))
import Mlatu.Report qualified as Report
import Mlatu.Term
import Mlatu.TypeDefinition qualified as TypeDefinition
import Mlatu.Vocabulary qualified as Vocabulary
import Optics
import Relude hiding (Compose)

fragment :: Fragment.Fragment () -> M ()
fragment f = do
  for_ (view Fragment.declarations f) declaration
  for_ (view Fragment.definitions f) definition
  for_ (view Fragment.metadata f) metadata
  for_ (view Fragment.types f) typeDefinition
  pass

declaration :: Declaration.Declaration -> M ()
declaration _ = pass

definition :: Definition.Definition () -> M ()
definition d = do
  term (view Definition.body d)
  pass

term :: Term () -> M ()
term t = go $ decompose t
  where
    go :: [Term ()] -> M ()
    go = \case
      [] -> pass
      ((Push _ v1 _) : (Push _ v2 _) : _)
        | v1 == v2 ->
          report $
            Report.makeWarning $ Report.UseCommon (origin t) dupName
      (_t : ts) -> go ts

    dupName = Qualified Vocabulary.global "dup"

metadata :: Metadata.Metadata -> M ()
metadata _ = pass

typeDefinition :: TypeDefinition.TypeDefinition -> M ()
typeDefinition _ = pass
