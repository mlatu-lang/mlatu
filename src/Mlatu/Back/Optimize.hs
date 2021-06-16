{-# LANGUAGE PatternSynonyms #-}

module Mlatu.Back.Optimize (rewrite) where

import Control.Arrow ((***))
import Mlatu.Back.AST (Expr (..), Pattern (..), VarIdent, mkAnd, pattern ECallCouple, pattern ECouple, pattern ESetCouple, pattern ESetVar)

rewritePat :: Pattern -> Pattern
rewritePat (PCons h t) = PCons (rewritePat h) (rewritePat t)
rewritePat PNil = PNil
rewritePat (PAtom a) = PAtom a
rewritePat (PInt i) = PInt i
rewritePat (PTuple ps) = PTuple (rewritePat <$> ps)
rewritePat (PVar v) = PVar v
rewritePat (PWhen pat guard) = case (rewritePat pat, rewrite guard) of
  (pat, EAtom "True") -> pat
  (PWhen pat guard1, guard2) -> PWhen pat (rewriteOp guard1 "andalso" guard2)
  (pat, guard) -> PWhen pat guard

rewrite :: Expr -> Expr
rewrite = \case
  (ECase scrutinee cases) -> rewriteCase scrutinee cases
  (ECallFun name args) -> rewriteCall name args
  (EVar name) -> EVar name
  (ESet pat expr) -> rewriteSet pat expr
  (EAnd xs) -> rewriteAnd xs
  ENil -> ENil
  (ECons h t) -> rewriteCons h t
  (EAtom atom) -> EAtom atom
  (EInt i) -> EInt i
  (EString s) -> EString s
  (EOp left op right) -> rewriteOp left op right
  (ETuple xs) -> rewriteTuple xs
  (EFun name arity) -> EFun name arity
  (EIf xs) -> rewriteIf xs

unify :: Expr -> Pattern -> Maybe [Expr]
unify (ECons h1 t1) (PCons h2 t2) = liftA2 (<>) (unify h1 h2) (unify t1 t2)
unify (EInt i1) (PInt i2) | i1 == i2 = Just []
unify (EAtom i1) (PAtom i2) | i1 == i2 = Just []
unify (ETuple es) (PTuple ps) = asum <$> zipWithM unify es ps
unify (EVar v1) (PVar v2) | v1 == v2 = Just []
unify x (PVar v) = Just [ESetVar v x]
unify _ _ = Nothing

rewriteCase :: Expr -> [(Pattern, Expr)] -> Expr
rewriteCase scrutinee cases =
  let final = case (rewrite scrutinee, (rewritePat *** rewrite) <$> cases) of
        (scrutinee, (p, e) : _) | Just es <- unify scrutinee p -> rewrite (mkAnd (es ++ [e]))
        (scrutinee, _ : (p, e) : _) | Just es <- unify scrutinee p -> rewrite (mkAnd (es ++ [e]))
        (scrutinee, _ : _ : (p, e) : _) | Just es <- unify scrutinee p -> rewrite (mkAnd (es ++ [e]))
        (scrutinee, _ : _ : _ : (p, e) : _) | Just es <- unify scrutinee p -> rewrite (mkAnd (es ++ [e]))
        (scrutinee, cases) ->
          ECase scrutinee $
            asum
              ( ( \case
                    (p, EIf guards) -> (first (PWhen p)) <$> guards
                    (p, b) -> [(p, b)]
                )
                  <$> cases
              )
   in final

rewriteCall :: Text -> [Expr] -> Expr
rewriteCall name args = case (name, rewrite <$> args) of
  ("not", [EAtom "true"]) -> EAtom "false"
  ("not", [EAtom "false"]) -> EAtom "true"
  ("hd", [ECons x _]) -> x
  ("tl", [ECons _ xs]) -> xs
  ("succ", [EInt n]) -> EInt (n + 1)
  ("pred", [EInt n]) -> EInt (if n == 0 then 0 else n - 1)
  ("sub", [EInt a, EInt b]) -> EInt (if a < b then 0 else a - b)
  (name, args) -> ECallFun name args

rewriteSet :: Pattern -> Expr -> Expr
rewriteSet pat expr = ESet pat (rewrite expr)

rewriteAnd :: [Expr] -> Expr
rewriteAnd xs = mkAnd (go xs)
  where
    go [] = []
    go (x : xs) = case rewrite x of
      EAnd ys -> go (xs <> ys)
      ESetVar "_" _ -> go xs
      ESetVar v e -> case (rewrite e : xs) of
        (EInt _ : xs) -> go (replaceVar (v, e) <$> xs)
        (EAtom _ : xs) -> go (replaceVar (v, e) <$> xs)
        (ETuple _ : xs) -> go (replaceVar (v, e) <$> xs)
        (EVar _ : xs) -> go (replaceVar (v, e) <$> xs)
        (EOp {} : xs) -> go (replaceVar (v, e) <$> xs)
        (ECons _ _ : xs) -> go (replaceVar (v, e) <$> xs)
        (_ : ECase (EVar v') cases : xs) | v == v' -> go (ECase e cases : xs)
        (_ : xs) -> (ESetVar v e) : (go xs)
      ESetCouple (PVar x) (PVar y) expr -> case xs of
        (ESetCouple a b (ECallCouple name (EVar x') (EVar y')) : xs) | x == x' && y == y' -> go (ESetCouple a b (ECallFun name [expr]) : xs)
        (ECouple (EVar x') (EVar y') : xs) | x == x' && y == y' -> go (expr : xs)
        _ -> (ESetCouple (PVar x) (PVar y) expr) : (go xs)
      x -> x : (go xs)

rewriteCons :: Expr -> Expr -> Expr
rewriteCons h t = ECons (rewrite h) (rewrite t)

rewriteOp :: Expr -> Text -> Expr -> Expr
rewriteOp left op right = case (rewrite left, op, rewrite right) of
  (EOp lLeft "+" lRight, "+", right) -> rewriteOp lLeft "+" (EOp lRight "+" right)
  (EOp lLeft "*" lRight, "*", right) -> rewriteOp lLeft "*" (EOp lRight "*" right)
  (x, "+", EInt 0) -> x
  (EInt 0, "+", x) -> x
  (x, "-", EInt 0) -> x
  (x, "*", EInt 1) -> x
  (EInt 1, "*", x) -> x
  (EInt i1, "+", EInt i2) -> EInt (i1 + i2)
  (EInt i1, "-", EInt i2) -> EInt (i1 - i2)
  (EInt i1, "*", EInt i2) -> EInt (i1 * i2)
  (EInt i1, "/", EInt i2) -> EInt (if i2 == 0 then 0 else i1 `div` i2)
  (EInt i1, ">", EInt i2) -> if i1 > i2 then EAtom "True" else EAtom "False"
  (EInt i1, "<", EInt i2) -> if i1 < i2 then EAtom "True" else EAtom "False"
  (EInt i1, ">=", EInt i2) -> if i1 >= i2 then EAtom "True" else EAtom "False"
  (EInt i1, "=<", EInt i2) -> if i1 <= i2 then EAtom "True" else EAtom "False"
  (EInt i1, "=:=", EInt i2) -> if i1 == i2 then EAtom "True" else EAtom "False"
  (EInt i1, "=/=", EInt i2) -> if i1 /= i2 then EAtom "True" else EAtom "False"
  (EAtom first, "and", x) -> if first == "true" then x else EAtom "false"
  (EAtom first, "or", x) -> if first == "false" then x else EAtom "true"
  (EAtom first, "xor", EAtom second)
    | (first == "true") /= (second == "true") -> EAtom "true"
    | otherwise -> EAtom "false"
  (x, op, y) -> EOp x op y

rewriteTuple :: [Expr] -> Expr
rewriteTuple = ETuple . fmap rewrite

rewriteIf :: [(Expr, Expr)] -> Expr
rewriteIf xs = case (rewrite *** rewrite) <$> xs of
  ((EAtom "true", expr) : _) -> expr
  (_ : (EAtom "true", expr) : _) -> expr
  (_ : _ : (EAtom "true", expr) : _) -> expr
  (_ : _ : _ : (EAtom "true", expr) : _) -> expr
  (_ : _ : _ : _ : (EAtom "true", expr) : _) -> expr
  xs -> EIf xs

replaceVar :: (VarIdent, Expr) -> Expr -> Expr
replaceVar (name, val) expr = case expr of
  (ECase scrutinee cases) -> ECase (replace scrutinee) ((replacePat *** replace) <$> cases)
  (ECallFun name args) -> ECallFun name (replace <$> args)
  (EVar n) | n == name -> val
  (ESet pat expr) -> ESet (replacePat pat) (replace expr)
  (EAnd xs) -> EAnd (replace <$> xs)
  (ECons h t) -> ECons (replace h) (replace t)
  (EOp left op right) -> EOp (replace left) op (replace right)
  (ETuple xs) -> ETuple (replace <$> xs)
  (EIf xs) -> EIf ((replace *** replace) <$> xs)
  _ -> expr
  where
    replace = replaceVar (name, val)

    replacePat = \case
      PCons h t -> PCons (replacePat h) (replacePat t)
      PVar n | n == name, Just r <- exprToPat val -> r
      PTuple pats -> PTuple (replacePat <$> pats)
      PWhen pat expr -> PWhen (replacePat pat) (replace expr)
      pat -> pat

exprToPat :: Expr -> Maybe Pattern
exprToPat (EInt i) = Just (PInt i)
exprToPat (EAtom a) = Just (PAtom a)
exprToPat (EVar v) = Just (PVar v)
exprToPat (ETuple es) = PTuple <$> traverse exprToPat es
exprToPat ENil = Just PNil
exprToPat (ECons h t) = liftA2 PCons (exprToPat h) (exprToPat t)
exprToPat _ = Nothing
