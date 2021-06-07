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
import Data.ByteString qualified as ByteString
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
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
import Mlatu.Pretty (printInstantiated, printQualified)
import Mlatu.Term (Specialness (..), Term (..), Value (..), decompose)
import Mlatu.Type (Type (..))
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Vocabulary
import Optics
import Relude hiding (Compose, Type, get)
import Relude.Unsafe qualified as Unsafe
import Text.Printf (printf)

instance IsString a => IsString (Codegen a) where
  fromString = pure . fromString

generate :: Dictionary -> Maybe Qualified -> IO ByteString
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
    ( "-module(mlatu).\n -export([main/0]).\n main() -> "
        <> firstKey
        <> "([], []).\n"
        <> ByteString.concat funs
    )

type WordMap = Map ByteString WordEntry

type FunIdent = ByteString

type AtomIdent = ByteString

type VarIdent = ByteString

type OpIdent = ByteString

data Pattern
  = PList [Pattern]
  | PVar VarIdent
  | PAtom AtomIdent
  | PInt Int
  | PTuple [Pattern]
  deriving (Ord, Eq, Show)

data EFun = MkFun FunIdent [VarIdent] Expr
  deriving (Ord, Eq, Show)

data Expr
  = ECase Expr [(Pattern, Expr)]
  | ECallFun FunIdent [Expr]
  | EGetVar VarIdent
  | ESetVar Pattern Expr
  | EOrElse Expr Expr
  | EAndAlso Expr Expr
  | EList [Expr]
  | EAtom AtomIdent
  | EInt Int
  | EString String
  | EOp Expr OpIdent Expr
  | ETuple [Expr]
  | EFun FunIdent Int
  | EIf [(Expr, Expr)]
  deriving (Ord, Eq, Show)

serPattern :: Pattern -> ByteString
serPattern (PList xs) =
  "[ "
    <> ( case xs of
           [] -> ""
           [x] -> serPattern x
           _ -> ByteString.concat (intersperse "," (serPattern <$> Unsafe.init xs)) <> "|" <> serPattern (Unsafe.last xs)
       )
    <> " ]"
serPattern (PVar var) = var
serPattern (PAtom a) = a
serPattern (PInt i) = show i
serPattern (PTuple xs) = "{" <> ByteString.concat (intersperse "," (serPattern <$> xs)) <> "}"

serExpr :: Expr -> ByteString
serExpr (ECase scrutinee cases) =
  "( case " <> serExpr scrutinee <> " of "
    <> ByteString.concat (intersperse ";" ((\(pattern, body) -> serPattern pattern <> " -> " <> serExpr body) <$> cases))
    <> " end ) "
serExpr (ECallFun name args) = name <> "(" <> ByteString.concat (intersperse "," (serExpr <$> args)) <> " )"
serExpr (EGetVar var) = var
serExpr (ESetVar var expr) = serPattern var <> " = " <> serExpr expr
serExpr (EOrElse x y) = serExpr x <> " ; " <> serExpr y
serExpr (EAndAlso x y) = serExpr x <> " , " <> serExpr y
serExpr (EList xs) =
  "[ "
    <> ( case xs of
           [] -> ""
           [x] -> serExpr x
           _ -> ByteString.concat (intersperse "," (serExpr <$> Unsafe.init xs)) <> "|" <> serExpr (Unsafe.last xs)
       )
    <> " ]"
serExpr (EAtom a) = a
serExpr (EIf xs) =
  "( if " <> ByteString.concat (intersperse ";" ((\(cond, body) -> serExpr cond <> " -> " <> serExpr body) <$> xs)) <> " end )"
serExpr (EInt i) = show i
serExpr (EFun name arity) = "fun " <> name <> "/" <> show arity
serExpr (EString s) = "\"" <> encodeUtf8 s <> "\""
serExpr (EOp left op right) = serExpr left <> op <> serExpr right
serExpr (ETuple xs) = "{ " <> ByteString.concat (intersperse "," (serExpr <$> xs)) <> " }"

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

entryErl :: Codegen ByteString
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
          case termErl body (Just (do
                rest <- getRestVar
                closure <- getClosureVar
                pure (ETuple [EGetVar rest, EGetVar closure]))) of
            Just action -> (\body -> i <> "(Rest0, Closure0) ->" <> serExpr body <> ".\n") <$> action
            Nothing -> pure ""
        _ -> pure ""

termErl :: Term Type -> Maybe (Codegen Expr) -> Maybe (Codegen Expr)
termErl x = goTerms (decompose x)
  where
    goTerms :: [Term Type] -> Maybe (Codegen Expr) -> Maybe (Codegen Expr)
    goTerms x after = case x of
      [] -> Nothing
      (Push _ _ (Text x) : Word _ _ (QualifiedName (Global "extern")) _ : rest) ->
        Just (intrinsic x (goTerms rest after))
      (Push _ _ (Name name) : NewClosure _ _ size : rest) -> Just $ do
        let names = zipWith (\name num -> name <> show num) (replicate size "Element") [0 ..]
        currentClosure <- getClosureVar
        newClosure <- incClosureVar
        currentRest <- getRestVar
        newRest <- incRestVar
        ( \expr ->
            ECase
              (EGetVar currentRest)
              [ ( PList (PVar <$> (names ++ ["Tail"])),
                  EAndAlso (ESetVar (PVar newRest) (EGetVar "Tail")) (expr)
                )
              ]
          )
          <$> andMaybe
            ( ESetVar
                (PVar newClosure)
                ( EList
                    [ ETuple [EFun (erlifyQ name) 2, EList (EGetVar <$> (reverse names))],
                      EGetVar currentClosure
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
        pushE [EGetVar localName] (goTerms rest after)
      (Push _ _ (Closed (ClosureIndex i)) : rest) -> Just $ do
        closure <- getClosureVar
        pushE [ECallFun "lists:n" [EGetVar closure, EInt i]] (goTerms rest after)
      (Word _ _ (QualifiedName (Qualified _ "cmp")) [TypeConstructor _ "nat"] : rest) ->
        Just $ cmp (goTerms rest after)
      (Word _ _ (QualifiedName name) ts : rest) ->
        Just $ word name ts (goTerms rest after)
      (Lambda _ _ _ _ body : rest) -> Just $ contained $ do
        local <- incLocal
        expr <- modifyE (PList [PVar ("Local" <> show local), PVar "Tail"]) (EGetVar "Tail") (termErl body after)
        andMaybe expr (goTerms rest Nothing)
      (Match _ _ cases (_, Right body) : rest) -> Just $ do
        current <- getRestVar
        cs <- traverse (\c -> caseErl c (goTerms rest after)) cases
        e <- case termErl body (goTerms rest after) of
          Just action -> (\e -> Just (PAtom "_", e)) <$> contained action
          Nothing -> pure Nothing
        pure (ECase (EGetVar current) (catMaybes (cs ++ [e])))
      (Match _ _ cases (_, Left _) : rest) -> Just $ do
        current <- getRestVar
        cs <- catMaybes <$> traverse (\c -> caseErl c (goTerms rest after)) cases
        e <- (PAtom "_",) <$> contained (word (Global "abort-now") [] (goTerms rest after))
        pure (ECase (EGetVar current) (cs ++ [e]))
      _ -> after

inTodo :: ByteString -> Codegen (Maybe WordEntry)
inTodo bs = Map.lookup bs <$> getToDo

inDone :: ByteString -> Codegen (Maybe WordEntry)
inDone bs = Map.lookup bs <$> getDone

word :: Qualified -> [Type] -> Maybe (Codegen Expr) -> Codegen Expr
word (Qualified _ "pred") _ after =
  modifyE
    (PList [PVar "PredHead", PVar "PredTail"])
    (EList [EOp (EGetVar "PredHead") "-" (EInt 1), EGetVar "PredTail"])
    after
word (Qualified _ "-") _ after =
  modifyE
    (PList [PVar "SubOne", PVar "SubTwo", PVar "SubTail"])
    (EList [EOp (EGetVar "SubOne") "-" (EGetVar "SubTwo"), EGetVar "SubTail"])
    after
word (Qualified _ "+") _ after =
  modifyE
    (PList [PVar "AddOne", PVar "AddTwo", PVar "AddTail"])
    (EList [EOp (EGetVar "AddOne") "-" (EGetVar "AddTwo"), EGetVar "AddTail"])
    after
word (Qualified _ "*") _ after =
  modifyE
    (PList [PVar "MulOne", PVar "MulTwo", PVar "MulTail"])
    (EList [EOp (EGetVar "MulOne") "*" (EGetVar "MulTwo"), EGetVar "MulTail"])
    after
word (Qualified _ "/") _ after =
  modifyE
    (PList [PVar "DivOne", PVar "DivTwo", PVar "DivTail"])
    (EList [EOp (EGetVar "DevOne") "*" (EGetVar "DivTwo"), EGetVar "DivTail"])
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
  Just action -> EAndAlso expr <$> action

pushE :: [Expr] -> Maybe (Codegen Expr) -> Codegen Expr
pushE newHeads after = do
  current <- EGetVar <$> getRestVar
  new <- incRestVar
  andMaybe (ESetVar (PVar new) (EList (newHeads ++ [current]))) after

modifyE :: Pattern -> Expr -> Maybe (Codegen Expr) -> Codegen Expr
modifyE pattern expr after = do
  current <- getRestVar
  new <- PVar <$> incRestVar
  case after of
    Nothing -> pure (ECase (EGetVar current) [(pattern, ESetVar new expr)])
    Just action -> (\e -> ECase (EGetVar current) [(pattern, EAndAlso (ESetVar new expr) e)]) <$> action

callWord :: Bool -> ByteString -> WordEntry -> Maybe (Codegen Expr) -> Codegen Expr
callWord b name e@(Entry.WordEntry _ _ _ _ _ (Just body)) after = case decompose body of
  [New _ _ (ConstructorIndex 0) 0 NatLike] -> pushE [EInt 0] after
  [New _ _ (ConstructorIndex 1) 1 NatLike] ->
    modifyE
      (PList [PVar "SuccHead", PVar "SuccTail"])
      (EList [EOp (EGetVar "SuccHead") "+" (EInt 1), EGetVar "SuccTail"])
      after
  [New _ _ (ConstructorIndex 1) 1 ListLike] -> pushE [EList []] after
  [New _ _ (ConstructorIndex 1) 2 ListLike] ->
    modifyE
      (PList [PVar "ListHead", PVar "ListTail", PVar "ConsTail"])
      (EList [EList [EGetVar "ListHead", EGetVar "ListTail"], EGetVar "ConsTail"])
      after
  [New _ _ (ConstructorIndex i) 0 NonSpecial] -> pushE [EInt i] after
  [New _ _ (ConstructorIndex i) size NonSpecial] -> do
    let names = zipWith (\name num -> name <> show num) (replicate size "Element") [0 ..]
    modifyE
      (PList (PVar <$> (names ++ ["Tail"])))
      (EList [ETuple (EInt i : (EGetVar <$> (reverse names))), EGetVar "ConsTail"])
      after
  _ -> do
    when b $ modifyToDo $ Map.insert name e
    rest <- getRestVar
    closure <- getClosureVar
    newRest <- PVar <$> incRestVar
    newClosure <- PVar <$> incClosureVar
    andMaybe
      ( ESetVar
          (PTuple [newRest, newClosure])
          (ECallFun name [EGetVar rest, EGetVar closure])
      )
      after

intrinsic :: Text -> Maybe (Codegen Expr) -> Codegen Expr
intrinsic text after = case text of
  "call" ->
    modifyE
      (PList [PTuple [PVar "CallName", PVar "CallClosure"], PVar "CallTail"])
      (ECallFun "CallName" [EGetVar "CallTail", EGetVar "CallClosure"])
      after
  "abort" -> do
    current <- getRestVar
    let expr = ECallFun "erlang:exit" [EGetVar "AbortMessage"]
    ( \expr ->
        ECase
          (EGetVar current)
          [(PList [PVar "AbortMessage", PVar "_"], expr)]
      )
      <$> andMaybe expr after
  "drop" ->
    modifyE
      (PList [PVar "_", PVar "DropTail"])
      (EGetVar "DropTail")
      after
  "swap" ->
    modifyE
      (PList [PVar "SwapFirst", PVar "SwapSecond", PVar "SwapTail"])
      (EList [EGetVar "SwapSecond", EGetVar "SwapFirst", EGetVar "SwapTail"])
      after
  "dup" ->
    modifyE
      (PList [PVar "DupHead", PVar "DupTail"])
      (EList [EGetVar "DupHead", EGetVar "DupHead", EGetVar "DupTail"])
      after
  "cmp-char" -> cmp after
  "show-nat" ->
    modifyE
      (PList [PVar "NatToShow", PVar "Show"])
      (EList [ECallFun "erlang:integer_to_string" [EGetVar "NatToShow"], EGetVar "Show"])
      after
  "read-nat" -> do
    some <- pushE [EGetVar "Int"] $ Just $ word (Global "some") [] Nothing
    none <- word (Global "none") [] Nothing
    current <- getRestVar
    new <- PVar <$> incRestVar
    let expr =
          EAndAlso
            ( ESetVar
                (PVar "ReadOutput")
                ( ECase
                    (EGetVar "Readable")
                    [ (PTuple [PAtom "error", PVar "_"], none),
                      (PTuple [PVar "Int", PVar "_"], some)
                    ]
                )
            )
            (ESetVar new (EList [EGetVar "ReadOutput", EGetVar "ReadTail"]))
    ( \expr ->
        ECase
          (EGetVar current)
          [ ( PList [PVar "Readable", PVar "ReadTail"],
              expr
            )
          ]
      )
      <$> andMaybe expr after
  "write-stdout" -> do
    modifyE
      (PList [PVar "ToWrite", PVar "WriteTail"])
      (EList [EGetVar "WriteTail"])
      $ Just $ andMaybe (ECallFun "io:fread" [EAtom "standard_io", EGetVar "ToWrite", EList []]) after
  "write-stderr" ->
    modifyE
      (PList [PVar "ToWrite", PVar "WriteTail"])
      (EList [EGetVar "WriteTail"])
      $ Just $
        andMaybe
          ( ECallFun
              "io:fread"
              [EAtom "standard_error", EGetVar "ToWrite", EList []]
          )
          after
  "read-line" ->
    andMaybe
      ( ESetVar
          (PTuple [PAtom "ok", PVar "ReadOutput"])
          (ECallFun "io::fwrite" [EString "", EString "~s"])
      )
      (Just (pushE [EGetVar "ReadOutput"] after))
  x -> error ("No such intrinsic: " <> show x)

cmp :: Maybe (Codegen Expr) -> Codegen Expr
cmp after = do
  m <- word (Global "more") [] after
  l <- word (Global "less") [] after
  e <- word (Global "equal") [] after
  modifyE
    (PList [PVar "First", PVar "Second", PVar "Tail"])
    ( EList
        [ EIf
            [ (EOp (EGetVar "First") ">" (EGetVar "Second"), m),
              (EOp (EGetVar "First") "<" (EGetVar "Second"), l),
              (EOp (EGetVar "First") "=:=" (EGetVar "Second"), e)
            ],
          EGetVar "Tail"
        ]
    )
    Nothing

caseErl :: (Origin, GeneralName, Term Type) -> Maybe (Codegen Expr) -> Codegen (Maybe (Pattern, Expr))
caseErl (_, QualifiedName name, caseBody) after = do
  dict <- getDict
  contained $ case Map.lookup (erlifyI (Instantiated name [])) dict of
    Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ _ (ConstructorIndex i) 0 NonSpecial] -> do
          new <- incRestVar
          let pattern = PList [PInt i, PVar new]
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (pattern,)) <$> action
        [New _ _ (ConstructorIndex i) 1 NonSpecial] -> do
          new <- incRestVar
          let pattern = PList [PTuple [PInt i, PList [PVar "Element"]], PVar new]
          expr <- pushE [EGetVar "Element"] (termErl caseBody after)
          pure (Just (pattern, expr))
        [New _ _ (ConstructorIndex i) size NonSpecial] -> do
          new <- incRestVar
          let names = zipWith (\name num -> name <> show num) (replicate size "Element") [0 ..]
          let pattern = PList [PTuple (PInt i : (PVar <$> names)), PVar new]
          expr <- pushE (EGetVar <$> (reverse names)) (termErl caseBody after)
          pure (Just (pattern, expr))
        [New _ _ (ConstructorIndex 0) 0 NatLike] -> do
          new <- incRestVar
          let pattern = PList [PInt 0, PVar new]
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (pattern,)) <$> action
        [New _ _ (ConstructorIndex 1) 1 NatLike] -> do
          new <- incRestVar
          let pattern = PList [PVar "Number", PVar new]
          expr <- pushE [EOp (EGetVar "Number") "-" (EInt 1)] (termErl caseBody after)
          pure (Just (pattern, expr))
        [New _ _ (ConstructorIndex 0) 0 ListLike] -> do
          new <- incRestVar
          let pattern = PList [PList [], PVar new]
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (pattern,)) <$> action
        [New _ _ (ConstructorIndex 1) 2 ListLike] -> do
          new <- incRestVar
          let pattern = PList [PList [PVar "ListHead", PVar "ListTail"], PVar new]
          expr <- pushE [EGetVar "ListHead", EGetVar "ListTail"] (termErl caseBody after)
          pure (Just (pattern, expr))
        _ -> pure Nothing
    _ -> pure Nothing
caseErl _ _ = pure Nothing

erlify :: ByteString -> ByteString
erlify txt =
  let string :: String = decodeUtf8 txt
      newString :: String =
        "m"
          <> concatMap
            ( \c ->
                if isAlphaNum c
                  then [c]
                  else if c == '-' then "_" else printf "%x" (ord c)
            )
            string
   in encodeUtf8 newString

erlifyQ :: Qualified -> ByteString
erlifyQ = erlify . show . printQualified

erlifyI :: Instantiated -> ByteString
erlifyI = erlify . show . printInstantiated
