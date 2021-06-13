-- |
-- Module      : Mlatu.Desugar.Infix
-- Description : Desugaring infix operators to postfix
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Middle.DesugarInfix
  ( desugar,
  )
where

import Mlatu.Base.Origin (Origin)
import Mlatu.Base.Origin qualified as Origin
import Mlatu.Front.Definition (Definition)
import Mlatu.Front.Definition qualified as Definition
import Mlatu.Front.Term (Term (..), Value (..))
import Mlatu.Front.Term qualified as Term
import Mlatu.Informer (M, ice, reportParseError)
import Text.Parsec (Parsec, SourcePos, (<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Expr qualified as Expr

type Rewriter a = Parsec [Term ()] () a

-- | Desugars infix operators into postfix calls in the body of a 'Definition',
-- according to the definitions and operator metadata in the 'Dictionary'.
desugar :: Definition () -> M (Definition ())
desugar definition = do
  let expression :: Rewriter (Term ())
      expression = Expr.buildExpressionParser [] operand
        where
          operand = (<?> "operand") $ do
            origin <- getTermOrigin
            results <- Parsec.many1 $
              termSatisfy $ \case
                Lambda {} -> False
                _otherTerm -> True
            pure $ Term.compose origin () results

      desugarTerms :: [Term ()] -> M (Term ())
      desugarTerms terms = do
        terms' <- traverse desugarTerm terms
        let expression' = infixExpression <* Parsec.eof
            infixExpression = do
              desugaredTerms <- many $ expression <|> lambda
              let origin = case desugaredTerms of
                    term : _ -> Term.origin term
                    _noTerms -> view Definition.origin definition
              pure $ Term.compose origin () desugaredTerms
        case Parsec.runParser expression' () "" terms' of
          Left parseError -> do
            reportParseError parseError
            let origin = case terms of
                  term : _ -> Term.origin term
                  _noTerms -> view Definition.origin definition
            pure $ Term.compose origin () terms
          Right result -> pure result

      desugarTerm :: Term () -> M (Term ())
      desugarTerm term = case term of
        Coercion {} -> pure term
        Compose _ a b -> desugarTerms (Term.decompose a ++ Term.decompose b)
        Generic {} ->
          ice
            "Mlatu.Desugar.Infix.desugar - generic expression should not appear before infix desugaring"
        Group a -> desugarTerms' a
        Lambda origin _ name _ body ->
          Lambda origin () name () <$> desugarTerms' body
        Match origin _ cases else_ ->
          Match origin () <$> traverse desugarCase cases <*> desugarElse else_
          where
            desugarCase (co, name, body) = (co,name,) <$> desugarTerms' body

            desugarElse (o, Left metadata) = pure (o, Left metadata)
            desugarElse (o, Right body) =
              (\x -> (o, Right x)) <$> desugarTerms' body
        New {} -> pure term
        NewClosure {} -> pure term
        Push origin _ value -> Push origin () <$> desugarValue value
        Word {} -> pure term

      desugarTerms' :: Term () -> M (Term ())
      desugarTerms' = desugarTerms . Term.decompose

      desugarValue :: Value () -> M (Value ())
      desugarValue value = case value of
        Capture names body -> Capture names <$> desugarTerms' body
        Character {} -> pure value
        Closed {} -> error "closed name should not appear before infix desugaring"
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
