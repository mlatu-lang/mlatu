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
          error
            "generic expression should not appear before scope resolution"
        recur Group {} =
          error
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
            ( map
                ( \(Case name a caseOrigin) ->
                    Case name (recur a) caseOrigin
                )
                cases
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
        recur (Word _ _ (LocalName index) _ origin) =
          Push () (scopeValue stack (Local index)) origin
        recur term@Word {} = term

    scopeValue :: [Int] -> Value () -> Value ()
    scopeValue _ Capture {} = error "capture should not appear before scope resolution"
    scopeValue _ value@Character {} = value
    scopeValue _ Closed {} = error "closed name should not appear before scope resolution"
    scopeValue _ value@Float {} = value
    scopeValue _ value@Integer {} = value
    scopeValue _ value@Local {} = value
    scopeValue _ value@Name {} = value
    scopeValue stack (Quotation body) = Capture (map ClosedLocal capturedNames) capturedTerm
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
  Coercion {} -> return term
  Compose _ a b -> Compose () <$> captureTerm a <*> captureTerm b
  Generic {} ->
    error
      "generic expression should not appear before scope resolution"
  Group {} ->
    error
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
      <$> mapM captureCase cases <*> captureElse else_ <*> pure origin
    where
      captureCase :: Case () -> Captured (Case ())
      captureCase (Case name a caseOrigin) =
        Case name <$> captureTerm a <*> pure caseOrigin

      captureElse :: Else () -> Captured (Else ())
      captureElse (DefaultElse a b) = return $ DefaultElse a b
      captureElse (Else a elseOrigin) =
        Else <$> captureTerm a <*> pure elseOrigin
  New {} -> return term
  NewClosure {} -> return term
  NewVector {} -> return term
  Push _ value origin -> Push () <$> captureValue value <*> pure origin
  Word {} -> return term

captureValue :: Value () -> Captured (Value ())
captureValue value = case value of
  Capture names term -> Capture <$> mapM close names <*> pure term
    where
      close :: Closed -> Captured Closed
      close original = case original of
        ClosedLocal index -> do
          closed <- closeLocal index
          return $ maybe original ClosedClosure closed
        ClosedClosure {} -> return original
  Character {} -> return value
  Closed {} -> return value
  Float {} -> return value
  Integer {} -> return value
  Local index -> do
    closed <- closeLocal index
    return $ maybe value Closed closed
  Name {} -> return value
  Quotation term ->
    let inside env = env {scopeStack = 0 : scopeStack env}
     in Quotation <$> local inside (captureTerm term)
  Text {} -> return value

closeLocal :: LocalIndex -> Captured (Maybe ClosureIndex)
closeLocal (LocalIndex index) = do
  stack <- asks scopeStack
  depth <- asks scopeDepth
  case stack of
    here : _
      | index >= here ->
        fmap Just $ addName $ LocalIndex $ index - depth
    _back -> return Nothing
  where
    addName :: LocalIndex -> Captured ClosureIndex
    addName name = do
      names <- lift get
      case elemIndex name names of
        Just existing -> return $ ClosureIndex existing
        Nothing -> do
          lift $ put $ names ++ [name]
          return $ ClosureIndex $ length names

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x : xs) = f x : xs
