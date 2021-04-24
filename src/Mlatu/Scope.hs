-- |
-- Module      : Mlatu.Scope
-- Description : Scope resolution
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Scope
  ( scope,
  )
where

import Data.List (elemIndex)
import Mlatu.Ice (ice)
import Mlatu.Name (Closed (..), ClosureIndex (..), GeneralName (..), LocalIndex (..))
import Mlatu.Term (Case (..), Else (..), Term (..), Value (..))
import Relude hiding (Compose)
import Relude.Extra (next)

-- | Whereas name resolution is concerned with resolving references to
-- definitions, scope resolution resolves local names to relative (De Bruijn)
-- indices, and converts 'Quotation's to explicit 'Capture's.
scope :: Term () -> Term ()
scope = scopeTerm [0]
  where
    scopeTerm :: [Int] -> Term () -> Term ()
    scopeTerm stack = recur
      where
        recur :: Term () -> Term ()
        recur term@Coercion {} = term
        recur (Compose _ a b) = Compose () (recur a) (recur b)
        recur Generic {} =
          ice
            "Mlatu.Scope.scope"
            "generic expression should not appear before scope resolution"
        recur Group {} =
          ice
            "Mlatu.Scope.scope"
            "group expression should not appear after infix desugaring"
        recur (Lambda _ name _ a origin) =
          Lambda
            ()
            name
            ()
            (scopeTerm (mapHead next stack) a)
            origin
        recur (Match hint _ cases else_ origin) =
          Match
            hint
            ()
            ( ( \(Case name a caseOrigin) ->
                  Case name (recur a) caseOrigin
              )
                <$> cases
            )
            ( ( \case
                  (DefaultElse a elseOrigin) -> (DefaultElse a elseOrigin)
                  (Else a elseOrigin) ->
                    Else (recur a) elseOrigin
              )
                else_
            )
            origin
        recur term@New {} = term
        recur term@NewClosure {} = term
        recur term@NewVector {} = term
        recur (Push _ value origin) = Push () (scopeValue stack value) origin
        recur (Word _ (LocalName index) _ origin) =
          Push () (scopeValue stack (Local index)) origin
        recur term@Word {} = term

    scopeValue :: [Int] -> Value () -> Value ()
    scopeValue _ Capture {} = ice "Mlatu.Scope.scope.scopeValue" "capture should not appear before scope resolution"
    scopeValue _ value@Character {} = value
    scopeValue _ Closed {} = ice "Mlatu.Scope.scope.scopeValue" "closed name should not appear before scope resolution"
    scopeValue _ value@Float {} = value
    scopeValue _ value@Integer {} = value
    scopeValue _ value@Local {} = value
    scopeValue _ value@Name {} = value
    scopeValue stack (Quotation body) = Capture (ClosedLocal <$> capturedNames) capturedTerm
      where
        capturedTerm :: Term ()
        capturedNames :: [LocalIndex]
        (capturedTerm, capturedNames) = runCapture stack' $ captureTerm scoped

        scoped :: Term ()
        scoped = scopeTerm stack' body

        stack' :: [Int]
        stack' = 0 : stack
    scopeValue _ value@Text {} = value

data ScopeEnv = ScopeEnv
  { scopeStack :: ![ScopeDepth],
    scopeDepth :: !ScopeDepth
  }

type ScopeDepth = Int

type Captured a = ReaderT ScopeEnv (State [LocalIndex]) a

runCapture :: [Int] -> Captured a -> (a, [LocalIndex])
runCapture stack =
  usingState []
    . usingReaderT ScopeEnv {scopeStack = stack, scopeDepth = 0}

captureTerm :: Term () -> Captured (Term ())
captureTerm term = case term of
  Coercion {} -> pure term
  Compose _ a b -> Compose () <$> captureTerm a <*> captureTerm b
  Generic {} ->
    ice
      "Mlatu.Scope.captureTerm"
      "generic expression should not appear before scope resolution"
  Group {} ->
    ice
      "Mlatu.Scope.captureTerm"
      "group expression should not appear after infix desugaring"
  Lambda _ name _ a origin ->
    let inside env =
          env
            { scopeStack = mapHead next (scopeStack env),
              scopeDepth = next (scopeDepth env)
            }
     in Lambda () name ()
          <$> local inside (captureTerm a) <*> pure origin
  Match hint _ cases else_ origin ->
    Match hint ()
      <$> traverse captureCase cases <*> captureElse else_ <*> pure origin
    where
      captureCase :: Case () -> Captured (Case ())
      captureCase (Case name a caseOrigin) =
        Case name <$> captureTerm a <*> pure caseOrigin

      captureElse :: Else () -> Captured (Else ())
      captureElse (DefaultElse a b) = pure $ DefaultElse a b
      captureElse (Else a elseOrigin) =
        Else <$> captureTerm a <*> pure elseOrigin
  New {} -> pure term
  NewClosure {} -> pure term
  NewVector {} -> pure term
  Push _ value origin -> Push () <$> captureValue value <*> pure origin
  Word {} -> pure term

captureValue :: Value () -> Captured (Value ())
captureValue value = case value of
  Capture names term -> Capture <$> traverse close names <*> pure term
    where
      close :: Closed -> Captured Closed
      close original = case original of
        ClosedLocal index -> do
          closed <- closeLocal index
          pure $ maybe original ClosedClosure closed
        ClosedClosure {} -> pure original
  Character {} -> pure value
  Closed {} -> pure value
  Float {} -> pure value
  Integer {} -> pure value
  Local index -> do
    closed <- closeLocal index
    pure $ maybe value Closed closed
  Name {} -> pure value
  Quotation term ->
    let inside env = env {scopeStack = 0 : scopeStack env}
     in Quotation <$> local inside (captureTerm term)
  Text {} -> pure value

closeLocal :: LocalIndex -> Captured (Maybe ClosureIndex)
closeLocal (LocalIndex index) = do
  stack <- asks scopeStack
  depth <- asks scopeDepth
  case stack of
    here : _
      | index >= here ->
        fmap Just $ addName $ LocalIndex $ index - depth
    _back -> pure Nothing
  where
    addName :: LocalIndex -> Captured ClosureIndex
    addName name = do
      names <- lift get
      case elemIndex name names of
        Just existing -> pure $ ClosureIndex existing
        Nothing -> do
          lift $ put $ names ++ [name]
          pure $ ClosureIndex $ length names

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x : xs) = f x : xs
