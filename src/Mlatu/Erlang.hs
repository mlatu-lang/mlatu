{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Mlatu.Interpret
-- Description : Simple interpreter
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Erlang
  ( generate,
  )
where

import Control.Monad.Loops (untilM)
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Mlatu.Definition (mainName)
import Mlatu.Dictionary (Dictionary, wordEntries)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry (WordEntry)
import Mlatu.Entry qualified as Entry
import Mlatu.Informer (runMlatu)
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Instantiated (Instantiated (..))
import Mlatu.Instantiated qualified as Instantiated
import Mlatu.Name (ClosureIndex (..), ConstructorIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Unqualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Pretty (printInstantiated, printQualified, printType)
import Mlatu.Term (Specialness (..), Term (..), Value (..), decompose)
import Mlatu.Type (Type (..))
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Vocabulary
import Optics
import Relude hiding (Compose, Type, get)
import Relude.Unsafe qualified as Unsafe
import System.Random (randomIO)
import Text.Printf (printf)

instance IsString a => IsString (Codegen a) where
  fromString = pure . fromString

generate :: Dictionary -> Maybe Qualified -> IO Text
generate dict mMain = do
  let firstKey = maybe "main" erlifyQ mMain
  let firstEntry = case Dictionary.lookupWord (Instantiated (fromMaybe mainName mMain) []) dict of
        Just e -> e
        Nothing -> error "Could not find main entry"
  funs <-
    evalCodegen
      (untilM entryErl (Map.null <$> getToDo))
      (one (firstKey, firstEntry), Map.empty, 0, 0, 0)
      (Map.mapKeys erlifyI (view wordEntries dict))
  pure
    ( "-module(mlatu).\n -export([main/1]).\n main(_) -> m"
        <> firstKey
        <> "({[], []}).\n"
        <> Text.concat funs
    )

type WordMap = Map Text WordEntry

type FunIdent = Text

type AtomIdent = Text

type VarIdent = Text

type OpIdent = Text

data Pattern
  = PList [Pattern]
  | PVar VarIdent
  | PAtom AtomIdent
  | PInt Int
  | PTuple [Pattern]
  | PWhen Pattern Expr
  deriving (Ord, Eq, Show)

data EFun = MkFun FunIdent [VarIdent] Expr
  deriving (Ord, Eq, Show)

data Expr
  = ECase Expr [(Pattern, Expr)]
  | ECallFun FunIdent [Expr]
  | EVar VarIdent
  | ESet Pattern Expr
  | EOrElse [Expr]
  | EAndAlso [Expr]
  | EList [Expr]
  | EAtom AtomIdent
  | EInt Int
  | EString String
  | EOp Expr OpIdent Expr
  | ETuple [Expr]
  | EFun FunIdent Int
  | EIf [(Expr, Expr)]
  deriving (Ord, Eq, Show)

serPattern :: Pattern -> Text
serPattern (PList xs) =
  "[ "
    <> ( case xs of
           [] -> ""
           [x] -> serPattern x
           _ -> Text.concat (intersperse " , " (serPattern <$> Unsafe.init xs)) <> " | " <> serPattern (Unsafe.last xs)
       )
    <> " ]"
serPattern (PVar var) = var
serPattern (PAtom a) = a
serPattern (PInt i) = show i
serPattern (PWhen p cond) = serPattern p <> " when " <> serExpr cond
serPattern (PTuple xs) = "{" <> Text.concat (intersperse " , " (serPattern <$> xs)) <> "}"

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

serExpr :: Expr -> Text
serExpr (ECase scrutinee cases) =
  " case " <> serExpr scrutinee <> " of "
    <> Text.concat (intersperse " ; " ((\(p, body) -> serPattern p <> " -> " <> serExpr body) <$> cases))
    <> " end "
serExpr (ECallFun name args) = name <> "(" <> Text.concat (intersperse " , " (serExpr <$> args)) <> " )"
serExpr (EVar var) = var
serExpr (ESet var expr) = serPattern var <> " = " <> serExpr expr
serExpr (EOrElse xs) = Text.concat (intersperse " ; " (serExpr <$> xs))
serExpr (EAndAlso xs) = Text.concat (intersperse " , " (serExpr <$> xs))
serExpr (EList xs) =
  "["
    <> ( case xs of
           [] -> ""
           [x] -> serExpr x
           _ -> Text.concat (intersperse " , " (serExpr <$> Unsafe.init xs)) <> " | " <> serExpr (Unsafe.last xs)
       )
    <> "]"
serExpr (EAtom a) = a
serExpr (EIf xs) =
  " if " <> Text.concat (intersperse " ; " ((\(cond, body) -> serExpr cond <> " -> " <> serExpr body) <$> xs)) <> " end "
serExpr (EInt i) = show i
serExpr (EFun name arity) = "fun " <> name <> "/" <> show arity
serExpr (EString s) = "\"" <> fromString s <> "\""
serExpr (EOp left op right) = "(" <> serExpr left <> " " <> op <> " " <> serExpr right <> ")"
serExpr (ETuple xs) = "{ " <> Text.concat (intersperse " , " (serExpr <$> xs)) <> " }"

rewrite :: Expr -> Expr
rewrite = \case
  (ECase scrutinee cases) -> rewriteCase scrutinee cases
  (ECallFun name args) -> rewriteCall name args
  (EVar name) -> EVar name
  (ESet pat expr) -> rewriteSet pat expr
  (EOrElse xs) -> rewriteOr xs
  (EAndAlso xs) -> rewriteAnd xs
  (EList xs) -> rewriteList xs
  (EAtom atom) -> EAtom atom
  (EInt i) -> EInt i
  (EString s) -> EString s
  (EOp left op right) -> rewriteOp left op right
  (ETuple xs) -> rewriteTuple xs
  (EFun name arity) -> EFun name arity
  (EIf xs) -> rewriteIf xs

rewriteCase scrutinee cases =
  ECase
    (rewrite scrutinee)
    ( ( \case
          (PCouple (PList [head, PVar first]) closure, ECase (EVar second) [((PList xs), b)])
            | first == second -> (PCouple (PList (head : xs)) closure, rewrite b)
          (p, b) -> (p, rewrite b)
      )
        <$> cases
    )

rewriteCall name args = ECallFun name (rewrite <$> args)

rewriteSet pat expr = ESet pat (rewrite expr)

rewriteOr xs = EOrElse (rewrite <$> xs)

rewriteAnd = EAndAlso . go
  where
    go = \case
      (ESetVar a (EList list) : ESetVar b (EList [head, EVar a']) : xs)
        | a == a' -> go (ESetVar b (EList (head : list)) : xs)
      (ESetCouple (PVar a) (PVar b) expr : ECallCouple name (EVar a') (EVar b') : xs)
        | a == a' && b == b' -> go (ECallFun name [expr] : xs)
      (ESetCouple (PVar a) (PVar b) expr : ECouple (EVar a') (EVar b') : xs)
        | a == a' && b == b' -> go (expr : xs)
      (ESetVar a expr : ECouple (EVar a') second : xs)
        | a == a' -> go ((ECouple expr second) : xs)
      (ESetVar a expr : ECouple first (EVar a') : xs)
        | a == a' -> go ((ECouple first expr) : xs)
      (x : xs) -> (rewrite x) : (go xs)
      [] -> []

rewriteList xs = EList (rewrite <$> xs)

rewriteOp left op right = case (rewrite left, op, rewrite right) of
  (EInt i1, "+", EInt i2) -> EInt (i1 + i2)
  (EInt i1, "-", EInt i2) -> EInt (i1 - i2)
  (EInt i1, "*", EInt i2) -> EInt (i1 * i2)
  (EInt i1, "/", EInt i2) -> EInt (i1 `div` i2)
  (EAtom first, "and", EAtom second)
    | first == "true" && second == "true" -> EAtom "true"
    | otherwise -> EAtom "false"
  (x, op, y) -> EOp x op y

rewriteTuple xs = ETuple (rewrite <$> xs)

rewriteIf xs = EIf ((\(a, b) -> (rewrite a, rewrite b)) <$> xs)

newtype Codegen a = Codegen (StateT (WordMap, WordMap, Int, Int, Int) (ReaderT WordMap IO) a)
  deriving (Monad, Functor, Applicative, MonadState (WordMap, WordMap, Int, Int, Int), MonadReader WordMap, MonadIO)

evalCodegen :: Codegen a -> (WordMap, WordMap, Int, Int, Int) -> WordMap -> IO a
evalCodegen (Codegen c) initialState = runReaderT (evalStateT c initialState)

getRestVar :: Codegen VarIdent
getRestVar = do
  num <- use _4
  pure ("Rest" <> show num)

incRestVar :: Codegen VarIdent
incRestVar = modifying _4 (+ 1) >> getRestVar

contained :: Codegen a -> Codegen a
contained act = do
  oldLocal <- use _3
  oldRest <- use _4
  oldClosure <- use _5
  result <- act
  assign _3 oldLocal
  assign _4 oldRest
  assign _5 oldClosure
  pure result

resetEverything :: Codegen ()
resetEverything = do
  assign _3 0
  assign _4 0
  assign _5 0

getClosureVar :: Codegen VarIdent
getClosureVar = do
  num <- use _5
  pure ("Closure" <> show num)

incClosureVar :: Codegen VarIdent
incClosureVar = modifying _5 (+ 1) >> getClosureVar

getLocal :: Codegen Int
getLocal = use _3

incLocal :: Codegen Int
incLocal = modifying _3 (+ 1) >> getLocal

getToDo :: Codegen WordMap
getToDo = use _1

setToDo :: WordMap -> Codegen ()
setToDo = assign _1

modifyToDo :: (WordMap -> WordMap) -> Codegen ()
modifyToDo = modifying _1

getDone :: Codegen WordMap
getDone = use _2

getDict :: Codegen WordMap
getDict = ask

modifyDone :: (WordMap -> WordMap) -> Codegen ()
modifyDone = modifying _2

newVar :: Codegen VarIdent
newVar = liftIO ((\(w :: Word32) -> "V" <> show w) <$> randomIO)

entryErl :: Codegen Text
entryErl = do
  result <- (Map.minViewWithKey <$> getToDo)
  case result of
    Nothing -> pure ""
    Just ((i, e), newMap) -> do
      setToDo newMap
      modifyDone (Map.insert i e)
      case e of
        (Entry.WordEntry _ _ _ _ _ (Just body)) -> do
          resetEverything
          case termErl
            body
            ( Just
                ( do
                    rest <- getRestVar
                    closure <- getClosureVar
                    pure (ETuple [EVar rest, EVar closure])
                )
            ) of
            Just action ->
              ( \x -> case rewrite x of
                  (ECase (EVar "Rest0") cases) ->
                    Text.concat
                      ( intersperse
                          " ;\n"
                          ( ( \(p, body) ->
                                "m" <> i <> "({" <> serPattern p
                                  <> ", Closure0}) ->"
                                  <> serExpr body
                            )
                              <$> cases
                          )
                      )
                      <> ".\n\n"
                  (ECase (EVar "Closure0") cases) ->
                    Text.concat
                      ( intersperse
                          " ;\n"
                          ( ( \(p, body) ->
                                "m" <> i <> "({Rest0," <> serPattern p
                                  <> "}) ->"
                                  <> serExpr body
                            )
                              <$> cases
                          )
                      )
                      <> ".\n\n"
                  body -> "m" <> i <> "({Rest0, Closure0}) ->" <> serExpr body <> ".\n\n"
              )
                <$> action
            Nothing -> pure ""
        _ -> pure ""

termErl :: Term Type -> Maybe (Codegen Expr) -> Maybe (Codegen Expr)
termErl x = goTerms (decompose x)
  where
    goTerms :: [Term Type] -> Maybe (Codegen Expr) -> Maybe (Codegen Expr)
    goTerms x after = case x of
      [] -> after
      (Push _ _ (Text x) : Word _ _ (QualifiedName (Global "extern")) _ : rest) ->
        Just (intrinsic x (goTerms rest after))
      (Push _ _ (Name name) : NewClosure _ _ 0 : rest) -> Just $ do
        _ <- contained (word name [] Nothing)
        pushE [ECouple (EFun ("m" <> erlifyQ name) 1) (EList [])] (goTerms rest after)
      (Push _ _ (Name name) : NewClosure _ _ size : rest) -> Just $ do
        _ <- contained (word name [] Nothing)
        elements <- replicateM size newVar
        let names = zipWith (\name num -> name <> show num) elements [0 ..]
        tail <- newVar
        modifyE
          (PList (PVar <$> (names ++ [tail])))
          (EList [ETuple [EFun ("m" <> erlifyQ name) 1, EList (EVar <$> (reverse names))], EVar tail])
          (goTerms rest after)
      (Word _ _ (QualifiedName (Qualified _ "zero")) _ : xs) -> Just $ do
        let go :: Int -> [Term a] -> (Int, [Term a])
            go n ((Word _ _ (QualifiedName (Qualified _ "succ")) _) : xs) = go (n + 1) xs
            go n xs = (n, xs)
            (s, rest) = go 0 xs
        pushE [EInt s] (goTerms rest after)
      (Group a : rest) -> goTerms (decompose a ++ rest) after
      (Push _ _ (Character c) : rest) -> Just $ pushE [EInt (ord c)] (goTerms rest after)
      (Push _ _ (Text txt) : rest) -> Just $ do
        let s = concatMap (\case '\n' -> "~n"; c -> [c]) (toString txt)
        pushE [EString s] (goTerms rest after)
      (Push _ _ (Local (LocalIndex i)) : rest) -> Just $ do
        ls <- getLocal
        let localName = "Local" <> show (ls - i)
        pushE [EVar localName] (goTerms rest after)
      (Push _ _ (Closed (ClosureIndex 0)) : rest) -> Just $ do
        closure <- getClosureVar
        pushE [ECallFun "hd" [EVar closure]] (goTerms rest after)
      (Push _ _ (Closed (ClosureIndex i)) : rest) -> Just $ do
        closure <- getClosureVar
        pushE [ECallFun "lists:n" [EVar closure, EInt i]] (goTerms rest after)
      (Word _ _ (QualifiedName (Qualified _ "cmp")) [TypeConstructor _ "nat"] : rest) ->
        Just $ cmp (goTerms rest after)
      (Word _ _ (QualifiedName name) ts : rest) ->
        Just $ word name ts (goTerms rest after)
      (Lambda _ _ _ _ body : rest) -> Just $
        contained $ do
          local <- incLocal
          tail <- newVar
          expr <- modifyE (PList [PVar ("Local" <> show local), PVar tail]) (EVar tail) (termErl body after)
          andMaybe expr (goTerms rest Nothing)
      (Match _ _ cases (_, Right body) : rest) -> Just $ do
        current <- getRestVar
        cs <- traverse (\c -> caseErl c (goTerms rest after)) cases
        e <- case termErl body (goTerms rest after) of
          Just action -> contained $ do
            new <- incRestVar
            e <- action
            pure (Just (PList [PVar "_", PVar new], e))
          Nothing -> pure Nothing
        pure (ECase (EVar current) (catMaybes (cs ++ [e])))
      (Match _ _ cases (_, Left _) : rest) -> Just $ do
        current <- getRestVar
        cs <- catMaybes <$> traverse (\c -> caseErl c (goTerms rest after)) cases
        e <- contained $ do
          new <- incRestVar
          e <- word (Global "abort-now") [] (goTerms rest after)
          pure (PList [PVar "_", PVar new], e)
        pure (ECase (EVar current) (cs ++ [e]))
      _ -> after

inTodo :: Text -> Codegen (Maybe WordEntry)
inTodo bs = Map.lookup bs <$> getToDo

inDone :: Text -> Codegen (Maybe WordEntry)
inDone bs = Map.lookup bs <$> getDone

word :: Qualified -> [Type] -> Maybe (Codegen Expr) -> Codegen Expr
word (Qualified _ "abort-now") _ after = do
  list <- newVar
  andMaybe (ECallFun "erlang:exit" [EString "abort called"]) after
word (Qualified _ "and") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (PList [PVar first, PVar second, PVar tail])
    (EList [EOp (EVar first) "and" (EVar second), EVar tail])
    after
word (Qualified _ "or") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (PList [PVar first, PVar second, PVar tail])
    (EList [EOp (EVar first) "or" (EVar second), EVar tail])
    after
word (Qualified _ "not") _ after = do
  head <- newVar
  tail <- newVar
  modifyE
    (PList [PVar head, PVar tail])
    (EList [ECallFun "not" [EVar head], EVar tail])
    after
word (Qualified _ "xor") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (PList [PVar first, PVar second, PVar tail])
    (EList [EOp (EVar first) "xor" (EVar second), EVar tail])
    after
word (Qualified _ "pred") _ after = do
  head <- newVar
  tail <- newVar
  modifyE
    (PList [PVar head, PVar tail])
    (EList [EOp (EVar head) "-" (EInt 1), EVar tail])
    after
word (Qualified _ "-") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (PList [PVar second, PVar first, PVar tail])
    (EList [EOp (EVar first) "-" (EVar second), EVar tail])
    after
word (Qualified _ "+") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (PList [PVar second, PVar first, PVar tail])
    (EList [EOp (EVar first) "+" (EVar second), EVar tail])
    after
word (Qualified _ "*") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (PList [PVar second, PVar first, PVar tail])
    (EList [EOp (EVar first) "*" (EVar second), EVar tail])
    after
word (Qualified _ "/") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (PList [PVar second, PVar first, PVar tail])
    (EList [EOp (EVar first) "/" (EVar second), EVar tail])
    after
word name args after = do
  let mangled = erlifyI $ Instantiated name args
  isInstantiated <- liftA2 (<|>) (inDone mangled) (inTodo mangled)
  case isInstantiated of
    Just e -> callWord False mangled e after
    _ -> do
      dict <- getDict
      case Map.lookup mangled dict of
        Just e -> callWord True mangled e after
        _ -> do
          let unMangled = erlifyI $ Instantiated name []
          case Map.lookup unMangled dict of
            Just (Entry.WordEntry a b c d e (Just body)) ->
              liftIO (fst <$> runMlatu (Instantiate.term TypeEnv.empty body args))
                >>= ( \case
                        Just body' -> callWord True mangled (Entry.WordEntry a b c d e (Just body')) after
                        Nothing -> error "Could not instantiate generic type"
                    )
            _ -> error "unknown word"

andMaybe :: Expr -> Maybe (Codegen Expr) -> Codegen Expr
andMaybe (EAndAlso es) = \case
  Nothing -> pure (EAndAlso es)
  Just action ->
    ( \case
        EAndAlso es' -> EAndAlso (es ++ es')
        e -> EAndAlso (es ++ [e])
    )
      <$> action
andMaybe expr = \case
  Nothing -> pure expr
  Just action ->
    ( \case
        EAndAlso es' -> EAndAlso (expr : es')
        e -> EAndAlso [expr, e]
    )
      <$> action

pushE :: [Expr] -> Maybe (Codegen Expr) -> Codegen Expr
pushE newHeads after = do
  current <- EVar <$> getRestVar
  new <- incRestVar
  andMaybe (ESetVar new (EList (newHeads ++ [current]))) after

modifyE :: Pattern -> Expr -> Maybe (Codegen Expr) -> Codegen Expr
modifyE p expr after = do
  current <- getRestVar
  new <- incRestVar
  case after of
    Nothing -> pure (ECase (EVar current) [(p, ESetVar new expr)])
    Just action -> (\e -> ECase (EVar current) [(p, EAndAlso [ESetVar new expr, e])]) <$> action

callWord :: Bool -> Text -> WordEntry -> Maybe (Codegen Expr) -> Codegen Expr
callWord b name e@(Entry.WordEntry _ _ _ _ _ (Just body)) after = case decompose body of
  [New _ _ (ConstructorIndex 0) 0 NatLike] -> pushE [EInt 0] after
  [New _ _ (ConstructorIndex 1) 1 NatLike] -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PList [PVar head, PVar tail])
      (EList [EOp (EVar head) "+" (EInt 1), EVar tail])
      after
  [New _ _ (ConstructorIndex 1) 1 ListLike] -> pushE [EList []] after
  [New _ _ (ConstructorIndex 1) 2 ListLike] -> do
    head <- newVar
    tail <- newVar
    rest <- newVar
    modifyE
      (PList [PVar head, PVar tail, PVar rest])
      (EList [EList [EVar head, EVar tail], EVar rest])
      after
  [New _ _ _ 0 NonSpecial] -> pushE [EAtom name] after
  [New _ _ _ size NonSpecial] -> do
    elements <- replicateM size newVar
    let names = zipWith (\name num -> name <> show num) elements [0 ..]
    tail <- newVar
    modifyE
      (PList (PVar <$> (names ++ [tail])))
      (EList [ETuple (EAtom name : (EVar <$> (reverse names))), EVar tail])
      after
  _ -> do
    when b $ modifyToDo $ Map.insert name e
    rest <- getRestVar
    closure <- getClosureVar
    newRest <- PVar <$> incRestVar
    newClosure <- PVar <$> incClosureVar
    andMaybe
      ( ESetCouple
          newRest
          newClosure
          (ECallCouple ("m" <> name) (EVar rest) (EVar closure))
      )
      after

intrinsic :: Text -> Maybe (Codegen Expr) -> Codegen Expr
intrinsic text after = case text of
  "call" -> do
    name <- newVar
    closure <- newVar
    tail <- newVar
    modifyE
      (PList [PCouple (PVar name) (PVar closure), PVar tail])
      (ECallCouple name (EVar tail) (EVar closure))
      after
  "abort" -> do
    current <- getRestVar
    andMaybe (ECallFun "erlang:exit" [ECallFun "hd" [EVar current]]) after
  "drop" -> do
    current <- getRestVar
    new <- incRestVar
    andMaybe (ESetVar new (ECallFun "tl" [EVar current])) after
  "swap" -> do
    first <- newVar
    second <- newVar
    tail <- newVar
    modifyE
      (PList [PVar first, PVar second, PVar tail])
      (EList [EVar second, EVar first, EVar tail])
      after
  "dup" -> do
    current <- getRestVar
    new <- incRestVar
    andMaybe (ESetVar new (EList [ECallFun "hd" [EVar current], EVar current])) after
  "cmp-char" -> cmp after
  "show-nat" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PList [PVar head, PVar tail])
      (EList [ECallFun "erlang:integer_to_list" [EVar head], EVar tail])
      after
  "read-nat" -> do
    output <- newVar
    number <- newVar
    readable <- newVar
    tail <- newVar
    some <- pushE [EVar number] $ Just $ word (Global "some") [] Nothing
    none <- word (Global "none") [] Nothing
    current <- getRestVar
    new <- incRestVar
    let expr =
          EAndAlso
            [ ESetVar
                output
                ( ECase
                    (EVar readable)
                    [ (PCouple (PAtom "error") (PVar "_"), none),
                      (PCouple (PVar "Int") (PVar "_"), some)
                    ]
                ),
              ESetVar new (EList [EVar output, EVar tail])
            ]
    (\expr -> ECase (EVar current) [(PList [PVar readable, PVar tail], expr)]) <$> andMaybe expr after
  "writeln-stdout" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PList [PVar head, PVar tail])
      (EList [EVar tail])
      $ Just $
        andMaybe
          ( EAndAlso
              [ ECallFun
                  "io:fwrite"
                  [EAtom "standard_io", EVar head, EList []],
                ECallFun "io:nl" [EAtom "standard_io"]
              ]
          )
          after
  "writeln-stderr" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PList [PVar head, PVar tail])
      (EList [EVar tail])
      $ Just $
        andMaybe
          ( EAndAlso
              [ ECallFun
                  "io:fwrite"
                  [EAtom "standard_error", EVar head, EList []],
                ECallFun "io:nl" [EAtom "standard_error"]
              ]
          )
          after
  "write-stdout" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PList [PVar head, PVar tail])
      (EList [EVar tail])
      $ Just $
        andMaybe
          ( ECallFun
              "io:fwrite"
              [EAtom "standard_io", EVar head, EList []]
          )
          after
  "write-stderr" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PList [PVar head, PVar tail])
      (EList [EVar tail])
      $ Just $
        andMaybe
          ( ECallFun
              "io:fwrite"
              [EAtom "standard_error", EVar head, EList []]
          )
          after
  "read-line" -> do
    output <- newVar
    andMaybe
      ( ESetCouple
          (PAtom "ok")
          (PVar output)
          (ECallFun "io::fwrite" [EString "", EString "~s"])
      )
      (Just (pushE [EVar output] after))
  x -> error ("No such intrinsic: " <> show x)

cmp :: Maybe (Codegen Expr) -> Codegen Expr
cmp after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  current <- getRestVar
  new <- incRestVar
  m <- contained (word (Global "more") [] after)
  l <- contained (word (Global "less") [] after)
  e <- contained (word (Global "equal") [] after)
  pure $
    ECase
      (EVar current)
      [ ( PList [PVar first, PVar second, PVar tail],
          EAndAlso
            [ ESetVar new (EVar tail),
              EIf
                [ (EOp (EVar first) "<" (EVar second), m),
                  (EOp (EVar first) ">" (EVar second), l),
                  (EOp (EVar first) "==" (EVar second), e)
                ]
            ]
        )
      ]

caseErl :: (Origin, GeneralName, Term Type) -> Maybe (Codegen Expr) -> Codegen (Maybe (Pattern, Expr))
caseErl (_, QualifiedName name, caseBody) after = do
  dict <- getDict
  let erlyName = erlifyQ name
  contained $ case Map.lookup erlyName dict of
    Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ _ _ 0 NonSpecial] -> do
          new <- incRestVar
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (PList [PAtom erlyName, PVar new],)) <$> action
        [New _ _ _ 1 NonSpecial] -> do
          new <- incRestVar
          element <- newVar
          expr <- pushE [EVar element] (termErl caseBody after)
          pure (Just (PList [PTuple [PAtom erlyName, PList [PVar element]], PVar new], expr))
        [New _ _ _ size NonSpecial] -> do
          new <- incRestVar
          elements <- replicateM size newVar
          let names = zipWith (\name num -> name <> show num) elements [0 ..]
          expr <- pushE (EVar <$> (reverse names)) (termErl caseBody after)
          pure (Just (PList [PTuple (PAtom erlyName : (PVar <$> names)), PVar new], expr))
        [New _ _ (ConstructorIndex 0) 0 NatLike] -> do
          new <- incRestVar
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (PList [PInt 0, PVar new],)) <$> action
        [New _ _ (ConstructorIndex 1) 1 NatLike] -> do
          new <- incRestVar
          number <- newVar
          expr <- pushE [EOp (EVar number) "-" (EInt 1)] (termErl caseBody after)
          pure (Just (PWhen (PList [PVar number, PVar new]) (EOp (EVar number) ">" (EInt 0)), expr))
        [New _ _ (ConstructorIndex 0) 0 ListLike] -> do
          new <- incRestVar
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (PList [PList [], PVar new],)) <$> action
        [New _ _ (ConstructorIndex 1) 2 ListLike] -> do
          new <- incRestVar
          head <- newVar
          tail <- newVar
          expr <- pushE [EVar head, EVar tail] (termErl caseBody after)
          pure (Just (PList [PList [PVar head, PVar tail], PVar new], expr))
        _ -> pure Nothing
    _ -> pure Nothing
caseErl _ _ = pure Nothing

erlify = Text.replace " " "_" . Text.replace "." "_" . Text.replace "-" "_"

erlifyQ :: Qualified -> Text
erlifyQ = erlify . show . printQualified

erlifyI :: Instantiated -> Text
erlifyI (Instantiated q []) = erlifyQ q
erlifyI (Instantiated q ts) = erlifyQ q <> "_" <> Text.concat (intersperse "_" ((erlify . show . printType) <$> ts)) <> "_"
