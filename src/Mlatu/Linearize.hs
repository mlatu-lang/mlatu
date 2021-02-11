-- |
-- Module      : Mlatu.Linearize
-- Description : Instrumentation of copies and drops
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Linearize
  ( linearize,
  )
where

import Data.List (maximum)
import Mlatu.Name (GeneralName (..), LocalIndex (..), Qualified (..))
import Mlatu.Operator qualified as Operator
import Mlatu.Origin (Origin)
import Mlatu.Term (Case (..), Else (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Type (Type)
import Mlatu.Vocabulary qualified as Vocabulary
import Relude hiding (Compose, Type)
import Relude.Extra (next)

-- | Linearization replaces all copies and drops with explicit invocations of
-- the @_::copy@ and @_::drop@ words. A value is copied if it appears twice or
-- more in its scope; it's dropped if it doesn't appear at all, or if an
-- explicit @drop@ is present due to an ignored local (@_@). If it only appears
-- once, it is moved, and no special word is invoked.
--
-- FIXME: This is experimental and subject to change.
linearize :: Term Type -> Term Type
linearize = snd . go []
  where
    go :: [Int] -> Term Type -> ([Int], Term Type)
    go counts0 term@Coercion {} = (counts0, term)
    go counts0 (Compose typ a b) =
      let (counts1, a') = go counts0 a
          (counts2, b') = go counts1 b
       in (counts2, Compose typ a' b')
    go counts0 (Generic name x body origin) =
      let (counts1, body') = go counts0 body
       in (counts1, Generic name x body' origin)
    go _ Group {} = error "group should not appear after desugaring"
    go counts0 (Lambda typ x varType body origin) =
      let (n : counts1, body') = go (0 : counts0) body
          body'' = case n of
            0 -> instrumentDrop origin varType body'
            1 -> body'
            _ -> instrumentCopy varType body'
       in (counts1, Lambda typ x varType body'' origin)
    -- FIXME: count usages for each branch & take maximum
    go counts0 (Match hint typ cases else_ origin) =
      let (counts1, mElse') = goElse counts0 else_
          (counts2, cases') =
            first (map maximum . transpose) $
              unzip $ map (goCase counts0) cases
       in (zipWith max counts1 counts2, Match hint typ cases' mElse' origin)
      where
        goCase :: [Int] -> Case Type -> ([Int], Case Type)
        goCase counts (Case name body caseOrigin) =
          let (counts1, body') = go counts body
           in (counts1, Case name body' caseOrigin)

        goElse :: [Int] -> Else Type -> ([Int], Else Type)
        goElse counts (Else body elseOrigin) =
          let (counts1, body') = go counts body
           in (counts1, Else body' elseOrigin)
    go counts0 term@New {} = (counts0, term)
    go counts0 term@NewClosure {} = (counts0, term)
    go counts0 term@NewVector {} = (counts0, term)
    go counts0 term@(Push _ (Local (LocalIndex index)) _) =
      let (h, t : ts) = splitAt index counts0
       in (h ++ next t : ts, term)
    go _ (Push _ Capture {} _) =
      error
        "pushing of capture should not appear after desugaring"
    go _ (Push _ Quotation {} _) =
      error
        "pushing of quotation should not appear after desugaring"
    go counts0 term@Push {} = (counts0, term)
    go counts0 term@Word {} = (counts0, term)

instrumentDrop :: Origin -> Type -> Term Type -> Term Type
instrumentDrop origin typ a =
  Term.compose
    todoTyped
    origin
    [ a,
      Push todoTyped (Local (LocalIndex 0)) origin,
      Word
        todoTyped
        Operator.Postfix
        (QualifiedName (Qualified Vocabulary.global "drop"))
        [typ]
        origin
    ]

instrumentCopy :: Type -> Term Type -> Term Type
instrumentCopy varType = go 0
  where
    go :: Int -> Term Type -> Term Type
    go _ term@Coercion {} = term
    go n (Compose typ a b) = Compose typ (go n a) (go n b)
    go n (Generic name i body origin) = Generic name i (go n body) origin
    go _ Group {} = error "group should not appear after desugaring"
    go n (Lambda typ name varType' body origin) =
      Lambda typ name varType' (go (next n) body) origin
    go n (Match hint typ cases else_ origin) =
      Match hint typ (map goCase cases) (goElse else_) origin
      where
        goCase :: Case Type -> Case Type
        goCase (Case name body caseOrigin) = Case name (go n body) caseOrigin

        goElse :: Else Type -> Else Type
        goElse (Else body elseOrigin) = Else (go n body) elseOrigin
    go _ term@New {} = term
    go _ term@NewClosure {} = term
    go _ term@NewVector {} = term
    go n term@(Push _ (Local (LocalIndex index)) origin)
      | index == n =
        Compose todoTyped term $
          Word
            todoTyped
            Operator.Postfix
            (QualifiedName (Qualified Vocabulary.global "copy"))
            [varType]
            origin
    go _ term@Push {} = term
    go _ term@Word {} = term

todoTyped :: a
todoTyped = error "TODO: generate typed terms"
