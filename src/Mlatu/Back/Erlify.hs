{-# LANGUAGE PatternSynonyms #-}

module Mlatu.Back.Erlify (entryErl, evalCodegen, erlifyI, erlifyQ, getToDo) where

import Data.Map qualified as Map
import Data.Text qualified as Text
import Mlatu.Back.AST
import Mlatu.Base.Name (ClosureIndex (..), ConstructorIndex (..), GeneralName (..), LocalIndex (..), Qualified (..))
import Mlatu.Base.Origin (Origin)
import Mlatu.Base.Type (Type (..))
import Mlatu.Base.Vocabulary (pattern Global)
import Mlatu.Front.Term (Specialness (..), Term (..), Value (..), decompose)
import Mlatu.Informer (ice, runMlatu)
import Mlatu.Middle.Entry (WordEntry)
import Mlatu.Middle.Entry qualified as Entry
import Mlatu.Middle.Instantiated (Instantiated (..))
import Mlatu.Pretty (printQualified, printType)
import Mlatu.TypeSystem.Instantiate qualified as Instantiate
import Mlatu.TypeSystem.TypeEnv qualified as TypeEnv
import System.Random (randomIO)

type WordMap = Map Text WordEntry

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

entryErl :: Codegen (Maybe EFun)
entryErl = do
  result <- (Map.minViewWithKey <$> getToDo)
  case result of
    Nothing -> pure Nothing
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
            Just action -> Just . MkFun ("m" <> i) <$> action
            Nothing -> pure Nothing
        _ -> pure Nothing

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
        pushE (ECouple (EFun ("m" <> erlifyQ name) 1) ENil) (goTerms rest after)
      (Push _ _ (Name name) : NewClosure _ _ size : rest) -> Just $ do
        _ <- contained (word name [] Nothing)
        elements <- replicateM size newVar
        let names = zipWith (\name num -> name <> show num) elements [(0 :: Int) ..]
        tail <- newVar
        modifyE
          (foldr PCons (PVar tail) (PVar <$> names))
          (ECons (ETuple [EFun ("m" <> erlifyQ name) 1, foldr ECons ENil (EVar <$> (reverse names))]) (EVar tail))
          (goTerms rest after)
      (Word _ _ (QualifiedName (Qualified _ "zero")) _ : xs) -> Just $ do
        let go :: Int -> [Term a] -> (Int, [Term a])
            go n ((Word _ _ (QualifiedName (Qualified _ "succ")) _) : xs) = go (n + 1) xs
            go n xs = (n, xs)
            (s, rest) = go 0 xs
        pushE (EInt s) (goTerms rest after)
      (Group a : rest) -> goTerms (decompose a ++ rest) after
      (Push _ _ (Character c) : rest) -> Just $ pushE (EInt (ord c)) (goTerms rest after)
      (Push _ _ (Text txt) : rest) -> Just $ do
        let s = concatMap (\case '\n' -> "~n"; c -> [c]) (toString txt)
        pushE (EString s) (goTerms rest after)
      (Push _ _ (Local (LocalIndex i)) : rest) -> Just $ do
        ls <- getLocal
        let localName = "Local" <> show (ls - i)
        pushE (EVar localName) (goTerms rest after)
      (Push _ _ (Closed (ClosureIndex 0)) : rest) -> Just $ do
        closure <- getClosureVar
        pushE (ECallFun "hd" [EVar closure]) (goTerms rest after)
      (Push _ _ (Closed (ClosureIndex i)) : rest) -> Just $ do
        closure <- getClosureVar
        pushE (ECallFun "lists:n" [EVar closure, EInt i]) (goTerms rest after)
      (Word _ _ (QualifiedName name) ts : rest) ->
        Just $ word name ts (goTerms rest after)
      (Lambda _ _ _ _ body : rest) -> Just $
        contained $ do
          local <- incLocal
          tail <- newVar
          expr <- modifyE (PCons (PVar ("Local" <> show local)) (PVar tail)) (EVar tail) (termErl body after)
          andMaybe expr (goTerms rest Nothing)
      (Match _ _ cases (_, Right body) : rest) -> Just $ do
        current <- getRestVar
        cs <- traverse (\c -> caseErl c (goTerms rest after)) cases
        e <- case termErl body (goTerms rest after) of
          Just action -> contained $ do
            new <- incRestVar
            e <- action
            pure (Just (PCons (PVar "_") (PVar new), e))
          Nothing -> pure Nothing
        pure (ECase (EVar current) (catMaybes (cs ++ [e])))
      (Match _ _ cases (_, Left _) : rest) -> Just $ do
        current <- getRestVar
        cs <- catMaybes <$> traverse (\c -> caseErl c (goTerms rest after)) cases
        e <- contained $ do
          new <- incRestVar
          e <- word (Global "abort-now") [] (goTerms rest after)
          pure (PCons (PVar "_") (PVar new), e)
        pure (ECase (EVar current) (cs ++ [e]))
      _ -> after

inTodo :: Text -> Codegen (Maybe WordEntry)
inTodo bs = Map.lookup bs <$> getToDo

inDone :: Text -> Codegen (Maybe WordEntry)
inDone bs = Map.lookup bs <$> getDone

word :: Qualified -> [Type] -> Maybe (Codegen Expr) -> Codegen Expr
word (Qualified _ "eq") _ after = binaryBool "==" "=/=" after
word (Qualified _ "neq") _ after = binaryBool "=/=" "==" after
word (Qualified _ "gt") _ after = binaryBool ">" "=<" after
word (Qualified _ "ge") _ after = binaryBool ">=" "<" after
word (Qualified _ "lt") _ after = binaryBool "<" ">=" after
word (Qualified _ "le") _ after = binaryBool "=<" ">" after
word (Qualified _ "abort-now") _ after = andMaybe (ECallFun "erlang:exit" [EString "abort called"]) after
word (Qualified _ "and") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (foldr PCons (PVar tail) [PVar first, PVar second])
    (ECons (EOp (EVar second) "and" (EVar first)) (EVar tail))
    after
word (Qualified _ "or") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (foldr PCons (PVar tail) [PVar first, PVar second])
    (ECons (EOp (EVar second) "or" (EVar first)) (EVar tail))
    after
word (Qualified _ "not") _ after = do
  head <- newVar
  tail <- newVar
  modifyE
    (PCons (PVar head) (PVar tail))
    (ECons (ECallFun "not" [EVar head]) (EVar tail))
    after
word (Qualified _ "xor") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (foldr PCons (PVar tail) [PVar first, PVar second])
    (ECons (EOp (EVar second) "xor" (EVar first)) (EVar tail))
    after
word (Qualified _ "pred") _ after = do
  head <- newVar
  tail <- newVar
  modifyE
    (PCons (PVar head) (PVar tail))
    (ECons (EOp (EVar head) "-" (EInt 1)) (EVar tail))
    after
word (Qualified _ "-") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (foldr PCons (PVar tail) [PVar first, PVar second])
    (ECons (EOp (EVar second) "-" (EVar first)) (EVar tail))
    after
word (Qualified _ "+") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (foldr PCons (PVar tail) [PVar first, PVar second])
    (ECons (EOp (EVar second) "+" (EVar first)) (EVar tail))
    after
word (Qualified _ "*") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (foldr PCons (PVar tail) [PVar first, PVar second])
    (ECons (EOp (EVar second) "*" (EVar first)) (EVar tail))
    after
word (Qualified _ "/") _ after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  modifyE
    (foldr PCons (PVar tail) [PVar first, PVar second])
    (ECons (EOp (EVar second) "/" (EVar first)) (EVar tail))
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
andMaybe x = \case
  Nothing -> pure x
  Just action -> (\e -> mkAnd [x, e]) <$> action

pushE :: Expr -> Maybe (Codegen Expr) -> Codegen Expr
pushE head after = do
  current <- EVar <$> getRestVar
  new <- incRestVar
  andMaybe (ESetVar new (ECons head current)) after

pushEs :: [Expr] -> Maybe (Codegen Expr) -> Codegen Expr
pushEs heads after = do
  current <- EVar <$> getRestVar
  new <- incRestVar
  andMaybe (ESetVar new (foldr ECons current heads)) after

modifyE :: Pattern -> Expr -> Maybe (Codegen Expr) -> Codegen Expr
modifyE p expr after = do
  current <- getRestVar
  new <- incRestVar
  case after of
    Nothing -> pure (ECase (EVar current) [(p, ESetVar new expr)])
    Just action -> (\e -> ECase (EVar current) [(p, mkAnd [ESetVar new expr, e])]) <$> action

callWord :: Bool -> Text -> WordEntry -> Maybe (Codegen Expr) -> Codegen Expr
callWord b name e@(Entry.WordEntry _ _ _ _ _ (Just body)) after = case decompose body of
  [New _ _ (ConstructorIndex 0) 0 NatLike] -> pushE (EInt 0) after
  [New _ _ (ConstructorIndex 1) 1 NatLike] -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PCons (PVar head) (PVar tail))
      (ECons (EOp (EVar head) "+" (EInt 1)) (EVar tail))
      after
  [New _ _ (ConstructorIndex 1) 1 ListLike] -> pushE ENil after
  [New _ _ (ConstructorIndex 1) 2 ListLike] -> do
    head <- newVar
    tail <- newVar
    rest <- newVar
    modifyE
      (foldr PCons (PVar rest) [PVar head, PVar tail])
      (ECons (ECons (EVar head) (EVar tail)) (EVar rest))
      after
  [New _ _ _ 0 NonSpecial] -> pushE (EAtom name) after
  [New _ _ _ size NonSpecial] -> do
    elements <- replicateM size newVar
    let names = zipWith (\name num -> name <> show num) elements [(0 :: Int) ..]
    tail <- newVar
    modifyE
      (foldr PCons (PVar tail) (PVar <$> names))
      (ECons (ETuple (EAtom name : (EVar <$> (reverse names)))) (EVar tail))
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
callWord _ _ _ _ = ice "Calling word without body"

intrinsic :: Text -> Maybe (Codegen Expr) -> Codegen Expr
intrinsic text after = case text of
  "call" -> do
    name <- newVar
    closure <- newVar
    tail <- newVar
    modifyE
      (PCons (PCouple (PVar name) (PVar closure)) (PVar tail))
      (ECallCouple name (EVar tail) (EVar closure))
      after
  "abort" -> do
    head <- newVar
    tail <- newVar
    current <- getRestVar
    new <- getRestVar
    ( \e ->
        ECase
          (EVar current)
          [(PCons (PVar head) (PVar tail), e)]
      )
      <$> andMaybe (EAnd [ESetVar new (EVar tail), ECallFun "erlang:exit" [EVar head]]) after
  "drop" -> do
    current <- getRestVar
    new <- incRestVar
    andMaybe (ESetVar new (ECallFun "tl" [EVar current])) after
  "dup" -> do
    current <- getRestVar
    pushE (ECallFun "hd" [EVar current]) after
  "cmp" -> do
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
        [ ( foldr PCons (PVar tail) [PVar first, PVar second],
            EAnd
              [ ESetVar new (EVar tail),
                EIf
                  [ (EOp (EVar first) "<" (EVar second), m),
                    (EOp (EVar first) ">" (EVar second), l),
                    (EOp (EVar first) "==" (EVar second), e)
                  ]
              ]
          )
        ]
  "self" -> pushE (ECallFun "erlang:self" []) after
  "kill" -> do
    head <- newVar
    tail <- newVar
    current <- getRestVar
    new <- getRestVar
    ( \e ->
        ECase (EVar current) [(PCons (PVar head) (PVar tail), e)]
      )
      <$> andMaybe
        ( EAnd
            [ ESetVar new (EVar tail),
              ECallFun
                "erlang:exit"
                [ECallFun head [ECouple ENil ENil], EAtom "kill"]
            ]
        )
        after
  "show-nat" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PCons (PVar head) (PVar tail))
      (ECons (ECallFun "erlang:integer_to_list" [EVar head]) (EVar tail))
      after
  "writeln-stdout" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PCons (PVar head) (PVar tail))
      (EVar tail)
      $ Just $
        andMaybe
          ( EAnd
              [ ECallFun
                  "io:fwrite"
                  [EAtom "standard_io", EVar head, ENil],
                ECallFun "io:nl" [EAtom "standard_io"]
              ]
          )
          after
  "writeln-stderr" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PCons (PVar head) (PVar tail))
      (EVar tail)
      $ Just $
        andMaybe
          ( EAnd
              [ ECallFun
                  "io:fwrite"
                  [EAtom "standard_error", EVar head, ENil],
                ECallFun "io:nl" [EAtom "standard_error"]
              ]
          )
          after
  "write-stdout" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PCons (PVar head) (PVar tail))
      (EVar tail)
      $ Just $
        andMaybe
          ( ECallFun
              "io:fwrite"
              [EAtom "standard_io", EVar head, ENil]
          )
          after
  "write-stderr" -> do
    head <- newVar
    tail <- newVar
    modifyE
      (PCons (PVar head) (PVar tail))
      (EVar tail)
      $ Just $
        andMaybe
          ( ECallFun
              "io:fwrite"
              [EAtom "standard_error", EVar head, ENil]
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
      (Just (pushE (EVar output) after))
  x -> error ("No such intrinsic: " <> show x)

binaryBool :: OpIdent -> OpIdent -> Maybe (Codegen Expr) -> Codegen Expr
binaryBool tOp fOp after = do
  first <- newVar
  second <- newVar
  tail <- newVar
  current <- getRestVar
  new <- incRestVar
  t <- contained (pushE (EAtom "true") after)
  f <- contained (pushE (EAtom "false") after)
  pure $
    ECase
      (EVar current)
      [ ( foldr PCons (PVar tail) [PVar first, PVar second],
          EAnd
            [ ESetVar new (EVar tail),
              EIf
                [ (EOp (EVar second) tOp (EVar first), t),
                  (EOp (EVar second) fOp (EVar first), f)
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
            Just action -> (Just . (PCons (PAtom erlyName) (PVar new),)) <$> action
        [New _ _ _ 1 NonSpecial] -> do
          new <- incRestVar
          element <- newVar
          expr <- pushE (EVar element) (termErl caseBody after)
          pure (Just (PCons (PTuple [PAtom erlyName, PCons (PVar element) PNil]) (PVar new), expr))
        [New _ _ _ size NonSpecial] -> do
          new <- incRestVar
          elements <- replicateM size newVar
          let names = zipWith (\name num -> name <> show num) elements [(0 :: Int) ..]
          expr <- pushEs (EVar <$> (reverse names)) (termErl caseBody after)
          pure (Just (PCons (PTuple (PAtom erlyName : (PVar <$> names))) (PVar new), expr))
        [New _ _ (ConstructorIndex 0) 0 NatLike] -> do
          new <- incRestVar
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (PCons (PInt 0) (PVar new),)) <$> action
        [New _ _ (ConstructorIndex 1) 1 NatLike] -> do
          new <- incRestVar
          number <- newVar
          expr <- pushE (EOp (EVar number) "-" (EInt 1)) (termErl caseBody after)
          pure (Just (PWhen (PCons (PVar number) (PVar new)) (EOp (EVar number) ">" (EInt 0)), expr))
        [New _ _ (ConstructorIndex 0) 0 ListLike] -> do
          new <- incRestVar
          case termErl caseBody after of
            Nothing -> pure Nothing
            Just action -> (Just . (PCons PNil (PVar new),)) <$> action
        [New _ _ (ConstructorIndex 1) 2 ListLike] -> do
          new <- incRestVar
          head <- newVar
          tail <- newVar
          expr <- pushEs [EVar head, EVar tail] (termErl caseBody after)
          pure (Just (PCons (PCons (PVar head) (PVar tail)) (PVar new), expr))
        _ -> pure Nothing
    _ -> pure Nothing
caseErl _ _ = pure Nothing

erlify :: Text -> Text
erlify = Text.replace " " "_" . Text.replace "." "_" . Text.replace "-" "_"

erlifyQ :: Qualified -> Text
erlifyQ = erlify . show . printQualified

erlifyI :: Instantiated -> Text
erlifyI (Instantiated q []) = erlifyQ q
erlifyI (Instantiated q ts) = erlifyQ q <> "_" <> Text.concat (intersperse "_" ((erlify . show . printType) <$> ts)) <> "_"
