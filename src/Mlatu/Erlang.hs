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
  let firstKey = maybe "mmain" erlifyQ mMain
  let firstEntry = case Dictionary.lookupWord (Instantiated (fromMaybe mainName mMain) []) dict of
        Just e -> e
        Nothing -> error "Could not find main entry"
  funs <-
    evalCodegen
      (untilM entryErl (Map.null <$> getToDo))
      (one (firstKey, firstEntry), Map.empty, 0, 0, 0)
      (Map.mapKeys erlifyI (view wordEntries dict))
  pure
    ( "-module(mlatu).\n -export([main/1]).\n main(_) -> "
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
  | EOrElse Expr Expr
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
           _ -> Text.concat (intersperse "," (serPattern <$> Unsafe.init xs)) <> "|" <> serPattern (Unsafe.last xs)
       )
    <> " ]"
serPattern (PVar var) = var
serPattern (PAtom a) = a
serPattern (PInt i) = show i
serPattern (PWhen pattern cond) = serPattern pattern <> " when " <> serExpr cond
serPattern (PTuple xs) = "{" <> Text.concat (intersperse "," (serPattern <$> xs)) <> "}"

serExpr :: Expr -> Text
serExpr (ECase scrutinee cases) =
  " case " <> serExpr scrutinee <> " of "
    <> Text.concat (intersperse ";" ((\(pattern, body) -> serPattern pattern <> " -> " <> serExpr body) <$> cases))
    <> " end "
serExpr (ECallFun name args) = name <> "(" <> Text.concat (intersperse "," (serExpr <$> args)) <> " )"
serExpr (EVar var) = var
serExpr (ESet var expr) = serPattern var <> " = " <> serExpr expr
serExpr (EOrElse x y) = serExpr x <> " ; " <> serExpr y
serExpr (EAndAlso xs) = go xs
  where
    go [] = ""
    go [x] = serExpr x
    go ((ESet (PTuple [PVar setFirst, PVar setSecond]) expr) : (ESet pattern (ECallFun name [ETuple [EVar getFirst, EVar getSecond]])) : xs)
      | setFirst == getFirst && setSecond == getSecond = go (ESet pattern (ECallFun name [expr]) : xs)
    go ((ESet (PVar setVarName) expr) : (ESet pattern (ECallFun name [ETuple [firstElem, EVar getVarName]])) : xs)
      | setVarName == getVarName = go ((ESet pattern (ECallFun name [ETuple [firstElem, expr]])) : xs)
    go ((ESet (PVar setVarName) expr) : (ESet pattern (ECallFun name [ETuple [EVar getVarName, secondElem]])) : xs)
      | setVarName == getVarName = go ((ESet pattern (ECallFun name [ETuple [expr, secondElem]])) : xs)
    go ((ESet (PTuple [PVar setFirst, PVar setSecond]) expr) : (ECallFun name [ETuple [EVar getFirst, EVar getSecond]]) : xs)
      | setFirst == getFirst && setSecond == getSecond = go ((ECallFun name [expr]) : xs)
    go ((ESet (PVar setVarName) expr) : (ECallFun name [ETuple [firstElem, EVar getVarName]]) : xs)
      | setVarName == getVarName = go ((ECallFun name [ETuple [firstElem, expr]]) : xs)
    go ((ESet (PVar setVarName) expr) : (ECallFun name [ETuple [EVar getVarName, secondElem]]) : xs)
      | setVarName == getVarName = go ((ECallFun name [ETuple [expr, secondElem]]) : xs)
    go ((ESet (PTuple [PVar setFirst, PVar setSecond]) expr) : (ETuple [EVar getFirst, EVar getSecond]) : xs)
      | setFirst == getFirst && setSecond == getSecond = go (expr : xs)
    go ((ESet (PVar setVarName) expr) : (ETuple [firstElem, EVar getVarName]) : xs)
      | setVarName == getVarName = go (ETuple [firstElem, expr] : xs)
    go ((ESet (PVar setVarName) expr) : (ETuple [EVar getVarName, secondElem]) : xs)
      | setVarName == getVarName = go (ETuple [expr, secondElem] : xs)
    go (x : xs) = serExpr x <> "," <> go xs
serExpr (EList xs) =
  "[ "
    <> ( case xs of
           [] -> ""
           [x] -> serExpr x
           _ -> Text.concat (intersperse "," (serExpr <$> Unsafe.init xs)) <> "|" <> serExpr (Unsafe.last xs)
       )
    <> " ]"
serExpr (EAtom a) = a
serExpr (EIf xs) =
  " if " <> Text.concat (intersperse ";" ((\(cond, body) -> serExpr cond <> " -> " <> serExpr body) <$> xs)) <> " end "
serExpr (EInt i) = show i
serExpr (EFun name arity) = "fun " <> name <> "/" <> show arity
serExpr (EString s) = "\"" <> fromString s <> "\""
serExpr (EOp left op right) = "(" <> serExpr left <> op <> serExpr right <> ")"
serExpr (ETuple xs) = "{ " <> Text.concat (intersperse "," (serExpr <$> xs)) <> " }"

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
              ( \case
                  (ECase (EVar "Rest0") [(pattern, body)]) -> i <> "({" <> serPattern pattern <> ", Closure0}) ->" <> serExpr body <> ".\n"
                  body -> i <> "({Rest0, Closure0}) ->" <> serExpr body <> ".\n"
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
      (Push _ _ (Name name) : NewClosure _ _ size : rest) -> Just $ do
        elements <- replicateM size newVar
        let names = zipWith (\name num -> name <> show num) elements [0 ..]
        currentClosure <- getClosureVar
        newClosure <- incClosureVar
        currentRest <- getRestVar
        newRest <- incRestVar
        tail <- newVar
        ( \expr ->
            ECase
              (EVar currentRest)
              [ ( PList (PVar <$> (names ++ [tail])),
                  EAndAlso [ESet (PVar newRest) (EVar tail), expr]
                )
              ]
          )
          <$> andMaybe
            ( ESet
                (PVar newClosure)
                ( EList
                    [ ETuple [EFun (erlifyQ name) 2, EList (EVar <$> (reverse names))],
                      EVar currentClosure
                    ]
                )
            )
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
andMaybe expr = \case
  Nothing -> pure expr
  Just action -> (\e -> EAndAlso [expr, e]) <$> action

pushE :: [Expr] -> Maybe (Codegen Expr) -> Codegen Expr
pushE newHeads after = do
  current <- EVar <$> getRestVar
  new <- incRestVar
  andMaybe (ESet (PVar new) (EList (newHeads ++ [current]))) after

modifyE :: Pattern -> Expr -> Maybe (Codegen Expr) -> Codegen Expr
modifyE pattern expr after = do
  current <- getRestVar
  new <- PVar <$> incRestVar
  case after of
    Nothing -> pure (ECase (EVar current) [(pattern, ESet new expr)])
    Just action -> (\e -> ECase (EVar current) [(pattern, EAndAlso [ESet new expr, e])]) <$> action

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
      ( ESet
          (PTuple [newRest, newClosure])
          (ECallFun name [ETuple [EVar rest, EVar closure]])
      )
      after

intrinsic :: Text -> Maybe (Codegen Expr) -> Codegen Expr
intrinsic text after = case text of
  "call" -> do
    name <- newVar
    closure <- newVar
    tail <- newVar
    modifyE
      (PList [PTuple [PVar name, PVar closure], PVar tail])
      (ECallFun name [ETuple [EVar tail, EVar closure]])
      after
  "abort" -> do
    current <- getRestVar
    andMaybe (ECallFun "erlang:exit" [ECallFun "hd" [EVar current]]) after
  "drop" -> do
    current <- getRestVar
    new <- incRestVar
    andMaybe (ESet (PVar new) (ECallFun "tl" [EVar current])) after
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
    andMaybe (ESet (PVar new) (EList [ECallFun "hd" [EVar current], EVar current])) after
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
    new <- PVar <$> incRestVar
    let expr =
          EAndAlso
            [ ESet
                (PVar output)
                ( ECase
                    (EVar readable)
                    [ (PTuple [PAtom "error", PVar "_"], none),
                      (PTuple [PVar "Int", PVar "_"], some)
                    ]
                ),
              ESet new (EList [EVar output, EVar tail])
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
      ( ESet
          (PTuple [PAtom "ok", PVar output])
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
            [ ESet (PVar new) (EVar tail),
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
  contained $ case Map.lookup (erlifyI (Instantiated name [])) dict of
    Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ _ _ 0 NonSpecial] -> do
          new <- incRestVar
          let pattern = PList [PAtom erlyName, PVar new]
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (pattern,)) <$> action
        [New _ _ _ 1 NonSpecial] -> do
          new <- incRestVar
          element <- newVar
          let pattern = PList [PTuple [PAtom erlyName, PList [PVar element]], PVar new]
          expr <- pushE [EVar element] (termErl caseBody after)
          pure (Just (pattern, expr))
        [New _ _ _ size NonSpecial] -> do
          new <- incRestVar
          elements <- replicateM size newVar
          let names = zipWith (\name num -> name <> show num) elements [0 ..]
          let pattern = PList [PTuple (PAtom erlyName : (PVar <$> names)), PVar new]
          expr <- pushE (EVar <$> (reverse names)) (termErl caseBody after)
          pure (Just (pattern, expr))
        [New _ _ (ConstructorIndex 0) 0 NatLike] -> do
          new <- incRestVar
          let pattern = PList [PInt 0, PVar new]
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (pattern,)) <$> action
        [New _ _ (ConstructorIndex 1) 1 NatLike] -> do
          new <- incRestVar
          number <- newVar
          let pattern = PWhen (PList [PVar number, PVar new]) (EOp (EVar number) ">" (EInt 0))
          expr <- pushE [EOp (EVar number) "-" (EInt 1)] (termErl caseBody after)
          pure (Just (pattern, expr))
        [New _ _ (ConstructorIndex 0) 0 ListLike] -> do
          new <- incRestVar
          let pattern = PList [PList [], PVar new]
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (pattern,)) <$> action
        [New _ _ (ConstructorIndex 1) 2 ListLike] -> do
          new <- incRestVar
          head <- newVar
          tail <- newVar
          let pattern = PList [PList [PVar head, PVar tail], PVar new]
          expr <- pushE [EVar head, EVar tail] (termErl caseBody after)
          pure (Just (pattern, expr))
        _ -> pure Nothing
    _ -> pure Nothing
caseErl _ _ = pure Nothing

erlify = Text.replace "-" "_"

erlifyQ :: Qualified -> Text
erlifyQ = erlify . show . printQualified

erlifyI :: Instantiated -> Text
erlifyI (Instantiated q []) = erlifyQ q
erlifyI (Instantiated q ts) = erlifyQ q <> "__" <> Text.concat (intersperse "_" ((erlify . show . printType) <$> ts)) <> "__"
