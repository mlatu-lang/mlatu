-- |
-- Module      : Mlatu.Desugar.Infix
-- Description : Desugaring infix operators to postfix
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Desugar.Infix
  ( desugar,
  )
where

import Mlatu.Definition (Definition)
import Mlatu.Definition qualified as Definition
import Mlatu.Dictionary (Dictionary)
import Mlatu.Ice (ice)
import Mlatu.Informer (Informer (..))
import Mlatu.Monad (M)
import Mlatu.Origin (Origin)
import Mlatu.Origin qualified as Origin
import Mlatu.Report qualified as Report
import Mlatu.Term (Case (..), Else (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Optics
import Relude hiding (Compose)
import Text.Parsec (Parsec, SourcePos, (<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Expr qualified as Expr

type Rewriter a = Parsec [Term ()] () a

-- | Desugars infix operators into postfix calls in the body of a 'Definition',
-- according to the definitions and operator metadata in the 'Dictionary'.
desugar :: Dictionary -> Definition () -> M (Definition ())
desugar _ definition = do
  let expression :: Rewriter (Term ())
      expression = Expr.buildExpressionParser [] operand
        where
          operand = (<?> "operand") $ do
            origin <- getTermOrigin
            results <- Parsec.many1 $
              termSatisfy $ \case
                Lambda {} -> False
                _otherTerm -> True
            pure $ Term.compose () origin results

      desugarTerms :: [Term ()] -> M (Term ())
      desugarTerms terms = do
        terms' <- traverse desugarTerm terms
        let expression' = infixExpression <* Parsec.eof
            infixExpression = do
              desugaredTerms <- many $ expression <|> lambda
              let origin = case desugaredTerms of
                    term : _ -> Term.origin term
                    _noTerms -> view Definition.origin definition
              pure $ Term.compose () origin desugaredTerms
        case Parsec.runParser expression' () "" terms' of
          Left parseError -> do
            report $ Report.parseError parseError
            let origin = case terms of
                  term : _ -> Term.origin term
                  _noTerms -> view Definition.origin definition
            pure $ Term.compose () origin terms
          Right result -> pure result

      desugarTerm :: Term () -> M (Term ())
      desugarTerm term = case term of
        Coercion {} -> pure term
        Compose _ a b -> desugarTerms (Term.decompose a ++ Term.decompose b)
        Generic {} ->
          ice
            "Mlatu.Desugar.Infix.desugar - generic expression should not appear before infix desugaring"
        Group a -> desugarTerms' a
        Lambda _ name _ body origin ->
          Lambda () name ()
            <$> desugarTerms' body <*> pure origin
        Match hint _ cases else_ origin ->
          Match hint ()
            <$> traverse desugarCase cases <*> desugarElse else_ <*> pure origin
          where
            desugarCase :: Case () -> M (Case ())
            desugarCase (Case name body caseOrigin) =
              Case name <$> desugarTerms' body <*> pure caseOrigin

            desugarElse :: Else () -> M (Else ())
            desugarElse (DefaultElse metadata o) = pure $ DefaultElse metadata o
            desugarElse (Else body elseOrigin) =
              Else <$> desugarTerms' body <*> pure elseOrigin
        New {} -> pure term
        NewClosure {} -> pure term
        NewVector {} -> pure term
        Push _ value origin -> Push () <$> desugarValue value <*> pure origin
        Word {} -> pure term

      desugarTerms' :: Term () -> M (Term ())
      desugarTerms' = desugarTerms . Term.decompose

      desugarValue :: Value () -> M (Value ())
      desugarValue value = case value of
        Capture names body -> Capture names <$> desugarTerms' body
        Character {} -> pure value
        Closed {} -> error "closed name should not appear before infix desugaring"
        Float {} -> pure value
        Integer {} -> pure value
        Local {} -> error "local name should not appear before infix desugaring"
        Name {} -> pure value
        Quotation body -> Quotation <$> desugarTerms' body
        Text {} -> pure value

  desugared <- desugarTerms' $ view Definition.body definition
  pure $ set Definition.body desugared definition

getTermOrigin :: Rewriter Origin
getTermOrigin =
  Term.origin
    <$> Parsec.lookAhead (termSatisfy (const True))

lambda ::
  Rewriter
    (Term ())
lambda = termSatisfy $ \case
  Lambda {} -> True
  _nonLambda -> False

termSatisfy :: (Term () -> Bool) -> Rewriter (Term ())
termSatisfy predicate =
  Parsec.tokenPrim
    show
    advanceTerm
    (\token -> if predicate token then Just token else Nothing)

advanceTerm :: SourcePos -> t -> [Term a] -> SourcePos
advanceTerm _ _ (term : _) = Origin.begin $ Term.origin term
advanceTerm sourcePos _ _ = sourcePos
