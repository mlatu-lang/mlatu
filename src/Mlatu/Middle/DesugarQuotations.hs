-- |
-- Module      : Mlatu.Desugar.Quotations
-- Description : Lifting anonymous functions
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Middle.DesugarQuotations
  ( desugar,
  )
where

import Data.Foldable (foldrM)
import Data.Map qualified as Map
import Mlatu.Base.Name (Closed (..), Qualified (..), Qualifier, Unqualified (..))
import Mlatu.Base.Type (Type (..), Var (..))
import Mlatu.Front.Definition (Category (..), Merge (..))
import Mlatu.Front.Signature qualified as Signature
import Mlatu.Front.Term (Term (..), Value (..))
import Mlatu.Front.Term qualified as Term
import Mlatu.Informer (M)
import Mlatu.Middle.Dictionary (Dictionary)
import Mlatu.Middle.Dictionary qualified as Dictionary
import Mlatu.Middle.Entry qualified as Entry
import Mlatu.Middle.Instantiated (Instantiated (Instantiated))
import Mlatu.TypeSystem.Free qualified as Free
import Mlatu.TypeSystem.Infer (inferType0)
import Mlatu.TypeSystem.TypeEnv (TypeEnv)
import Mlatu.TypeSystem.TypeEnv qualified as TypeEnv

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
  pure (term', dictionary')
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
        pure (Compose typ a' b', tenv2)
      Generic origin name typ a -> do
        (a', tenv1) <- go tenv0 a
        pure (Generic origin name typ a', tenv1)
      Group {} -> error "group should not appear after infix desugaring"
      Lambda origin typ name varType a -> do
        let oldLocals = view TypeEnv.vs tenv0
            localEnv = over TypeEnv.vs (varType :) tenv0
        (a', tenv1) <- go localEnv a
        let tenv2 = set TypeEnv.vs oldLocals tenv1
        pure (Lambda origin typ name varType a', tenv2)
      Match origin typ cases else_ -> do
        (cases', tenv1) <-
          foldrM
            ( \(caseOrigin, name, a) (acc, tenv) -> do
                (a', tenv') <- go tenv a
                pure ((caseOrigin, name, a') : acc, tenv')
            )
            ([], tenv0)
            cases
        (else', tenv2) <- case else_ of
          (elseOrigin, Left a) -> pure ((elseOrigin, Left a), tenv1)
          (elseOrigin, Right a) -> do
            (a', tenv') <- go tenv1 a
            pure ((elseOrigin, Right a'), tenv')
        pure (Match origin typ cases' else', tenv2)
      New {} -> done
      NewClosure {} -> done
      Push origin _type (Capture closed a) -> do
        let types = mapMaybe (TypeEnv.getClosed tenv0) closed
            oldClosure = view TypeEnv.closure tenv0
            localEnv = set TypeEnv.closure types tenv0
        (a', tenv1) <- go localEnv a
        let tenv2 = set TypeEnv.closure oldClosure tenv1
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
                Entry.WordEntry
                  DefinedWord
                  DenyMerge
                  (Term.origin a')
                  Nothing
                  (Just (Signature.Type typ))
                  (Just a')
           in (l, Dictionary.insertWord (Instantiated name []) entry d)
        dict <- gets snd
        (typechecked, _) <-
          lift $
            inferType0 dict tenv2 Nothing $
              Term.compose origin () $
                (pushClosed <$> closed)
                  ++ [ Push origin () (Name name),
                       NewClosure origin () (length closed)
                     ]
        pure (typechecked, tenv2)
        where
          pushClosed :: Closed -> Term ()
          pushClosed name =
            Push
              origin
              ()
              ( case name of
                  ClosedLocal index -> Local index
                  ClosedClosure index -> Closed index
              )
      Push {} -> done
      Word {} -> done
      where
        done = pure (term, tenv0)
