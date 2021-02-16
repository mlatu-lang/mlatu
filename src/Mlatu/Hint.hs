module Mlatu.Hint (fragment) where

import Mlatu.Declaration qualified as Declaration
import Mlatu.Definition qualified as Definition
import Mlatu.Fragment qualified as Fragment
import Mlatu.Informer (Informer (..))
import Mlatu.Metadata qualified as Metadata
import Mlatu.Monad (K)
import Mlatu.Name (Qualified (..))
import Mlatu.Report qualified as Report
import Mlatu.Synonym qualified as Synonym
import Mlatu.Term qualified as Term
import Mlatu.TypeDefinition qualified as TypeDefinition
import Mlatu.Vocabulary qualified as Vocabulary
import Relude

fragment :: (Eq a) => Fragment.Fragment a -> K ()
fragment f = do
  forM_ (Fragment.declarations f) declaration
  forM_ (Fragment.definitions f) definition
  forM_ (Fragment.metadata f) metadata
  forM_ (Fragment.synonyms f) synonym
  forM_ (Fragment.types f) typeDefinition
  pass

declaration :: Declaration.Declaration -> K ()
declaration _ = pass

definition :: (Eq a) => Definition.Definition a -> K ()
definition d = do
  term (Definition.body d)
  pass

term :: (Eq a) => Term.Term a -> K ()
term t = go $ Term.decompose t
  where
    go :: (Eq a) => [Term.Term a] -> K ()
    go = \case
      [] -> pass
      (t1@Term.Push {} : t2@Term.Push {} : _)
        | t1 == t2 ->
          report $
            Report.makeWarning $ Report.UseCommon (Term.origin t) dup_name
      (_ : ts) -> go ts

    dup_name = Qualified Vocabulary.global "dup"

metadata :: Metadata.Metadata -> K ()
metadata _ = pass

synonym :: Synonym.Synonym -> K ()
synonym _ = pass

typeDefinition :: TypeDefinition.TypeDefinition -> K ()
typeDefinition _ = pass