-- |
-- Module      : Mlatu.Desugar.Quotations
-- Description : Lifting anonymous functions
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Desugar.Quotations
  ( desugar,
  )
where

import Data.Foldable (foldrM)
import Data.Map qualified as Map
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Free qualified as Free
import Mlatu.Infer (inferType0)
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Monad (M)
import Mlatu.Name (Closed (..), Qualified (..), Qualifier, Unqualified (..))
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Case (..), Else (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Type (Type (..), Var (..))
import Mlatu.TypeEnv (TypeEnv)
import Mlatu.TypeEnv qualified as TypeEnv
import Relude hiding (Compose, Type)
import Relude.Extra (next)

newtype LambdaIndex = LambdaIndex Int

-- | Lifts quotations in a 'Term' into top-level definitions, within the
-- vocabulary referenced by a 'Qualifier', adding them to the 'Dictionary'.
desugar ::
  Dictionary ->
  Qualifier ->
  Term Type ->
  M (Term Type, Dictionary)
desugar dictionary qualifier term0 = do
  ((term', _), (_, dictionary')) <-
    usingStateT (LambdaIndex 0, dictionary) $
      go TypeEnv.empty term0
  return (term', dictionary')
  where
    go ::
      TypeEnv ->
      Term Type ->
      StateT (LambdaIndex, Dictionary) M (Term Type, TypeEnv)
    go tenv0 term = case term of
      Coercion {} -> done
      Compose typ a b -> do
        (a', tenv1) <- go tenv0 a
        (b', tenv2) <- go tenv1 b
        return (Compose typ a' b', tenv2)
      Generic name typ a origin -> do
        (a', tenv1) <- go tenv0 a
        return (Generic name typ a' origin, tenv1)
      Group {} -> error "group should not appear after infix desugaring"
      Lambda typ name varType a origin -> do
        let oldLocals = TypeEnv.vs tenv0
            localEnv = tenv0 {TypeEnv.vs = varType : TypeEnv.vs tenv0}
        (a', tenv1) <- go localEnv a
        let tenv2 = tenv1 {TypeEnv.vs = oldLocals}
        return (Lambda typ name varType a' origin, tenv2)
      Match hint typ cases else_ origin -> do
        (cases', tenv1) <-
          foldrM
            ( \(Case name a caseOrigin) (acc, tenv) -> do
                (a', tenv') <- go tenv a
                return (Case name a' caseOrigin : acc, tenv')
            )
            ([], tenv0)
            cases
        (else', tenv2) <- case else_ of
          DefaultElse a elseOrigin -> return (DefaultElse a elseOrigin, tenv1)
          Else a elseOrigin -> do
            (a', tenv') <- go tenv1 a
            return (Else a' elseOrigin, tenv')
        return (Match hint typ cases' else' origin, tenv2)
      New {} -> done
      NewClosure {} -> done
      NewVector {} -> done
      Push _type (Capture closed a) origin -> do
        let types = mapMaybe (TypeEnv.getClosed tenv0) closed
            oldClosure = TypeEnv.closure tenv0
            localEnv = tenv0 {TypeEnv.closure = types}
        (a', tenv1) <- go localEnv a
        let tenv2 = tenv1 {TypeEnv.closure = oldClosure}
        LambdaIndex index <- gets fst
        let name =
              Qualified qualifier $
                Unqualified $ toText $ "lambda" ++ show index
        modify $ \(_, d) -> (LambdaIndex $ next index, d)
        let deducedType = Term.typ a
            typ =
              foldr addForall deducedType $
                Map.toList $ Free.tvks tenv2 deducedType
            addForall (i, (n, k)) = Forall origin (Var n i k)
        modify $ \(l, d) ->
          let entry =
                Entry.Word
                  Category.Word
                  Merge.Deny
                  (Term.origin a')
                  Nothing
                  (Just (Signature.Type typ))
                  (Just a')
           in (l, Dictionary.insert (Instantiated name []) entry d)
        dict <- gets snd
        (typechecked, _) <-
          lift $
            inferType0 dict tenv2 Nothing $
              Term.compose () origin $
                map pushClosed closed
                  ++ [ Push () (Name name) origin,
                       NewClosure () (length closed) origin
                     ]
        return (typechecked, tenv2)
        where
          pushClosed :: Closed -> Term ()
          pushClosed name =
            Push
              ()
              ( case name of
                  ClosedLocal index -> Local index
                  ClosedClosure index -> Closed index
              )
              origin
      Push {} -> done
      Word {} -> done
      where
        done = return (term, tenv0)
