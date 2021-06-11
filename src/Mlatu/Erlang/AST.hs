{-# LANGUAGE PatternSynonyms #-}

module Mlatu.Erlang.AST
  ( Pattern (..),
    Expr (..),
    EFun (..),
    FunIdent,
    AtomIdent,
    VarIdent,
    OpIdent,
    mkAnd,
    pattern ECouple,
    pattern ESetCouple,
    pattern PCouple,
    pattern ECallCouple,
    pattern ESetVar,
  )
where

import Relude

type FunIdent = Text

type AtomIdent = Text

type VarIdent = Text

type OpIdent = Text

data Pattern
  = PCons Pattern Pattern
  | PNil
  | PVar VarIdent
  | PAtom AtomIdent
  | PInt Int
  | PTuple [Pattern]
  | PWhen Pattern Expr
  deriving (Ord, Eq, Show)

data EFun = MkFun FunIdent Expr
  deriving (Ord, Eq, Show)

data Expr
  = ECase Expr [(Pattern, Expr)]
  | ECallFun FunIdent [Expr]
  | EVar VarIdent
  | ESet Pattern Expr
  | EAnd [Expr]
  | ECons Expr Expr
  | ENil
  | EAtom AtomIdent
  | EInt Int
  | EString String
  | EOp Expr OpIdent Expr
  | ETuple [Expr]
  | EFun FunIdent Int
  | EIf [(Expr, Expr)]
  deriving (Ord, Eq, Show)

pattern PCouple :: Pattern -> Pattern -> Pattern
pattern PCouple a b = PTuple [a, b]

pattern ECouple :: Expr -> Expr -> Expr
pattern ECouple a b = ETuple [a, b]

pattern ECallCouple :: FunIdent -> Expr -> Expr -> Expr
pattern ECallCouple n a b = ECallFun n [ECouple a b]

pattern ESetCouple :: Pattern -> Pattern -> Expr -> Expr
pattern ESetCouple a b expr = ESet (PCouple a b) expr

pattern ESetVar :: VarIdent -> Expr -> Expr
pattern ESetVar var expr = ESet (PVar var) expr

mkAnd :: [Expr] -> Expr
mkAnd xs = case go xs of
  [x] -> x
  xs -> EAnd xs
  where
    go [] = []
    go (EAnd xs : ys) = go (xs <> ys)
    go (x : xs) = x : (go xs)
