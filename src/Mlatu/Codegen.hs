-- |
-- Module      : Mlatu.Interpret
-- Description : Simple interpreter
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Codegen
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
import Text.Printf (printf)

instance IsString a => IsString (Codegen a) where
  fromString = pure . fromString

generate :: Dictionary -> Maybe Qualified -> Bool -> IO ByteString
generate dict mMain isOnline = do
  let firstKey = maybe "mmain" rustifyQualified mMain
  let firstEntry = case Dictionary.lookupWord (Instantiated (fromMaybe mainName mMain) []) dict of
        Just e -> e
        Nothing -> error "Could not find main entry"
  bs <-
    evalCodegen
      (untilM entryRs (Map.null <$> getToDo))
      (one (firstKey, firstEntry), Map.empty, 0)
      (Map.mapKeys rustifyInstantiated (view wordEntries dict), isOnline)
  pure
    ( "#![allow(warnings)] #[inline] fn get(stack: &mut Vec<Rep>) -> StackResult<Rep> { stack.pop().ok_or(CompilerError) }"
        <> ( if isOnline
               then "extern crate smallvec; use smallvec::*; type AVec = SmallVec<[Box<Rep>; 2]>;"
               else "type AVec = Vec<Rep>;"
           )
        <> " fn main() { match "
        <> firstKey
        <> "(&mut Vec::with_capacity(6), &Vec::new()) {\
           \Err(AbortCalled(s)) => eprintln!(\"Abort called: {}\", s),\
           \Err(CompilerError) => eprintln!(\"Internal compiler error\"),\
           \Err(IOError) => eprintln!(\"IO error\"),\
           \Ok(()) => {},\
           \} }\
           \ type StackFn = fn(&mut Vec<Rep>, &[Rep]) -> StackResult<()>;\
           \ type StackResult<T> = Result<T, Error>; \
           \ #[derive(Clone)] enum Rep { Closure(StackFn, Vec<Rep>), Algebraic(usize, AVec), Nat(usize), List(Vec<Rep>), Char(char), Text(String) } \
           \ #[derive(Clone)] enum Error { AbortCalled(String), CompilerError, IOError } \
           \ use Rep::*; use Error::*; "
        <> toBS (asum bs)
    )

type WordMap = Map ByteString WordEntry

data Block
  = FunBlock ByteString [Block]
  | UnwrapNat ByteString
  | MatchBlock ByteString [(ByteString, [Block])]
  | UnwrapChar ByteString
  | UnwrapText ByteString
  | UnwrapList ByteString
  | UnwrapClosure ByteString ByteString
  | UnwrapAlgebraic ByteString ByteString
  | Unwrap ByteString
  | PushNat ByteString
  | PushChar ByteString
  | PushText ByteString
  | PushList ByteString
  | PushClosure ByteString ByteString
  | PushAlgebraic ByteString ByteString
  | Push_ ByteString
  | Call ByteString
  | Let ByteString ByteString
  | Custom ByteString
  deriving (Ord, Eq, Show)

couple :: ByteString -> ByteString -> ByteString
couple a b = "(" <> a <> "," <> b <> ")"

tryNat :: ByteString -> ByteString
tryNat a = "if let Nat(a) = " <> a <> " { a } else { return Err(CompilerError); }"

tryList :: ByteString -> ByteString
tryList a = "if let List(a) = " <> a <> " { a } else { return Err(CompilerError); }"

tryChar :: ByteString -> ByteString
tryChar a = "if let Char(a) = " <> a <> " { a } else { return Err(CompilerError); }"

tryText :: ByteString -> ByteString
tryText a = "if let Text(a) = " <> a <> " { a } else { return Err(CompilerError); }"

tryClosure :: ByteString -> ByteString
tryClosure a = "if let Closure(a,b) = " <> a <> " { (a,b) } else { return Err(CompilerError); }"

tryAlgebraic :: ByteString -> ByteString
tryAlgebraic a = "if let Algebraic(a,b) = " <> a <> " { (a,b) } else { return Err(CompilerError); }"

get :: ByteString
get = "get(stack)?"

toBS :: [Block] -> ByteString
toBS = toBytes . optimize
  where
    optimize (Let aName aBody : Let bName bBody : rest)
      | aName == bName =
        optimize
          ( Let
              bName
              ( "{ let " <> aName <> " = "
                  <> aBody
                  <> "; "
                  <> bBody
                  <> " }"
              ) :
            rest
          )
    optimize (Push_ a : Unwrap b : rest) = optimize (Let b a : rest)
    optimize (Push_ a : UnwrapNat b : rest) = optimize (Let b (tryNat a) : rest)
    optimize (Push_ a : UnwrapList b : rest) = optimize (Let b (tryList a) : rest)
    optimize (Push_ a : UnwrapChar b : rest) = optimize (Let b (tryChar a) : rest)
    optimize (Push_ a : UnwrapText b : rest) = optimize (Let b (tryText a) : rest)
    optimize (Push_ a : UnwrapAlgebraic b1 b2 : rest) = optimize (Let (couple b1 b2) (tryAlgebraic a) : rest)
    optimize (Push_ a : UnwrapClosure b1 b2 : rest) = optimize (Let (couple b1 b2) (tryClosure a) : rest)
    optimize (PushNat a : Unwrap b : rest) = optimize (Let b ("Nat(" <> a <> ")") : rest)
    optimize (PushList a : Unwrap b : rest) = optimize (Let b ("List(" <> a <> ")") : rest)
    optimize (PushChar a : Unwrap b : rest) = optimize (Let b ("Char(" <> a <> ")") : rest)
    optimize (PushText a : Unwrap b : rest) = optimize (Let b ("Text(" <> a <> ")") : rest)
    optimize (PushAlgebraic a1 a2 : Unwrap b : rest) = optimize (Let b ("Algebraic" <> couple a1 a2) : rest)
    optimize (PushClosure a1 a2 : Unwrap b : rest) = optimize (Let b ("Closure" <> couple a1 a2) : rest)
    optimize (PushNat a : UnwrapNat b : rest) = optimize (Let b a : rest)
    optimize (PushList a : UnwrapList b : rest) = optimize (Let b a : rest)
    optimize (PushChar a : UnwrapChar b : rest) = optimize (Let b a : rest)
    optimize (PushText a : UnwrapText b : rest) = optimize (Let b a : rest)
    optimize (PushAlgebraic a1 a2 : UnwrapAlgebraic b1 b2 : rest) = optimize (Let b1 a1 : (Let b2 a2 : rest))
    optimize (PushClosure a1 a2 : UnwrapClosure b1 b2 : rest) = optimize (Let b1 a1 : (Let b2 a2 : rest))
    optimize (prior : Unwrap name : rest) = optimize (prior : Let name get : rest)
    optimize (prior : UnwrapNat name : rest) = optimize (prior : Let name (tryNat get) : rest)
    optimize (prior : UnwrapChar name : rest) = optimize (prior : Let name (tryChar get) : rest)
    optimize (prior : UnwrapText name : rest) = optimize (prior : Let name (tryText get) : rest)
    optimize (prior : UnwrapList name : rest) = optimize (prior : Let name (tryList get) : rest)
    optimize (prior : UnwrapClosure name1 name2 : rest) = optimize (prior : Let (couple name1 name2) (tryClosure get) : rest)
    optimize (prior : UnwrapAlgebraic name1 name2 : rest) = optimize (prior : Let (couple name1 name2) (tryAlgebraic get) : rest)
    optimize (prior : Unwrap name : rest) = optimize (prior : Let name get : rest)
    optimize (FunBlock name body : rest) = FunBlock name (optimize body) : optimize rest
    optimize (Unwrap name : rest) = optimize (Let name get : rest)
    optimize (UnwrapNat name : rest) = optimize (Let name (tryNat get) : rest)
    optimize (UnwrapChar name : rest) = optimize (Let name (tryChar get) : rest)
    optimize (UnwrapText name : rest) = optimize (Let name (tryText get) : rest)
    optimize (UnwrapList name : rest) = optimize (Let name (tryList get) : rest)
    optimize (UnwrapClosure name1 name2 : rest) = optimize (Let (couple name1 name2) (tryClosure get) : rest)
    optimize (UnwrapAlgebraic name1 name2 : rest) = optimize (Let (couple name1 name2) (tryAlgebraic get) : rest)
    optimize (Unwrap name : rest) = optimize (Let name get : rest)
    optimize (a : rest) = a : optimize rest
    optimize [] = []

    toBytes [] = ""
    toBytes (FunBlock name body : rest) =
      "#[inline] fn " <> name
        <> "(stack: &mut Vec<Rep>, closures: &[Rep]) -> StackResult<()> { "
        <> toBS body
        <> " Ok(()) } "
        <> toBytes rest
    toBytes (PushNat body : rest) = "stack.push(Nat(" <> body <> "));" <> toBytes rest
    toBytes (PushChar body : rest) = "stack.push(Char(" <> body <> "));" <> toBytes rest
    toBytes (PushText body : rest) = "stack.push(Text(" <> body <> "));" <> toBytes rest
    toBytes (PushList body : rest) = "stack.push(List(" <> body <> "));" <> toBytes rest
    toBytes (PushClosure body1 body2 : rest) =
      "stack.push(Closure" <> couple body1 body2 <> ");" <> toBytes rest
    toBytes (PushAlgebraic body1 body2 : rest) =
      "stack.push(Algebraic" <> couple body1 body2 <> ");" <> toBytes rest
    toBytes (Push_ body : rest) = "stack.push(" <> body <> ");" <> toBytes rest
    toBytes (MatchBlock x xs : rest) =
      "match " <> x <> " { "
        <> ByteString.concat ((\(binding, block) -> binding <> " => {" <> toBS block <> "},") <$> xs)
        <> " };"
        <> toBS rest
    toBytes (Call name : rest) = name <> "(stack, closures)?;" <> toBytes rest
    toBytes (Let a b : rest) = "let " <> a <> " = " <> b <> ";" <> toBytes rest
    toBytes (Custom body : rest) = body <> toBytes rest
    toBytes (x : rest) = error $ show x

newtype Codegen a = Codegen (StateT (WordMap, WordMap, Int) (ReaderT (WordMap, Bool) IO) a)
  deriving (Monad, Functor, Applicative, MonadState (WordMap, WordMap, Int), MonadReader (WordMap, Bool), MonadIO)

evalCodegen :: Codegen a -> (WordMap, WordMap, Int) -> (WordMap, Bool) -> IO a
evalCodegen (Codegen c) initialState = runReaderT (evalStateT c initialState)

getLocal :: Codegen Int
getLocal = use _3

modifyLocal :: (Int -> Int) -> Codegen ()
modifyLocal = modifying _3

setLocal :: Int -> Codegen ()
setLocal = assign _3

getToDo :: Codegen WordMap
getToDo = use _1

setToDo :: WordMap -> Codegen ()
setToDo = assign _1

modifyToDo :: (WordMap -> WordMap) -> Codegen ()
modifyToDo = modifying _1

getDone :: Codegen WordMap
getDone = use _2

isOnline :: Codegen Bool
isOnline = view _2 <$> ask

getDict :: Codegen WordMap
getDict = view _1 <$> ask

modifyDone :: (WordMap -> WordMap) -> Codegen ()
modifyDone = modifying _2

entryRs :: Codegen [Block]
entryRs =
  getToDo
    >>= ( ( \case
              Nothing -> pure []
              Just ((i, e), newMap) ->
                (setToDo newMap >> modifyDone (Map.insert i e))
                  >> ( case e of
                         (Entry.WordEntry _ _ _ _ _ (Just body)) -> do
                           setLocal 0
                           b <- termRs body
                           pure [FunBlock i b]
                         _ -> pure []
                     )
          )
            . Map.minViewWithKey
        )

termRs :: Term Type -> Codegen [Block]
termRs x = goTerms $ decompose x
  where
    goTerms = \case
      [] -> pure []
      (Push _ _ (Text x) : Word _ _ (QualifiedName (Global "extern")) _ : xs) ->
        liftA2 (<>) (intrinsic x) (goTerms xs)
      (Push _ _ (Name name) : NewClosure _ _ size : xs) ->
        ( ( case size of
              0 -> [PushClosure (rustifyQualified name) "Vec::new()"]
              _ ->
                [ Let "v" (vecBuilder size "vec"),
                  PushClosure (rustifyQualified name) "v"
                ]
          )
            <>
        )
          <$> goTerms xs
      (Word _ _ (QualifiedName (Qualified _ "zero")) _ : xs) -> do
        let go :: Int -> [Term a] -> (Int, [Term a])
            go n ((Word _ _ (QualifiedName (Qualified _ "succ")) _) : xs) = go (n + 1) xs
            go n rest = (n, rest)
            (s, rest) = go 0 xs
        (PushNat (show s) :) <$> goTerms rest
      (y : ys) -> liftA2 (<>) (goTerm y) (goTerms ys)
    goTerm = \case
      Group a -> termRs a
      Push _ _ (Character c) -> pure [PushChar $ "'" <> show c <> "'"]
      Push _ _ (Text txt) ->
        pure
          [ PushText
              ( "\""
                  <> encodeUtf8
                    ( concatMap
                        ( \case
                            '\n' -> "\\n"
                            c -> [c]
                        )
                        (toString txt)
                    )
                  <> "\".to_owned()"
              )
          ]
      Push _ _ (Local (LocalIndex i)) -> do
        ls <- getLocal
        pure [Push_ ("local" <> show ls <> ".clone()")]
      Push _ _ (Closed (ClosureIndex i)) -> pure [Push_ ("closures[" <> show i <> "].clone()")]
      Word _ _ (QualifiedName (Qualified _ "cmp")) [TypeConstructor _ "nat"] ->
        (\c -> [UnwrapNat "a", UnwrapNat "b", c]) <$> cmp "a" "b"
      Word _ _ (QualifiedName (Qualified _ "pred")) _ ->
        pure [UnwrapNat "a", PushNat "a-1"]
      Word _ _ (QualifiedName (Qualified _ "+")) _ ->
        pure [UnwrapNat "a", UnwrapNat "b", PushNat "a+b"]
      Word _ _ (QualifiedName (Qualified _ "-")) _ ->
        pure [UnwrapNat "a", UnwrapNat "b", PushNat "a-b"]
      Word _ _ (QualifiedName (Qualified _ "*")) _ ->
        pure [UnwrapNat "a", UnwrapNat "b", PushNat "a*b"]
      Word _ _ (QualifiedName (Qualified _ "/")) _ ->
        pure [UnwrapNat "a", UnwrapNat "b", PushNat "a/b"]
      Word _ _ (QualifiedName name) args -> word name args
      Lambda _ _ _ _ body -> do
        modifyLocal (+ 1)
        ls <- getLocal
        b <- termRs body
        modifyLocal (\x -> x - 1)
        pure $ Unwrap ("local" <> show ls) : b
      Match _ _ cases els -> do
        cs <- traverse caseRs cases
        e <- case els of
          (_, Right body) -> termRs body
          (_, Left _) -> word (Global "abort-now") []
        pure [Unwrap "scrutinee", MatchBlock "scrutinee" (catMaybes cs ++ [("_", e)])]
      _ -> pure []

inTodo :: ByteString -> Codegen (Maybe WordEntry)
inTodo bs = Map.lookup bs <$> getToDo

inDone :: ByteString -> Codegen (Maybe WordEntry)
inDone bs = Map.lookup bs <$> getDone

word :: Qualified -> [Type] -> Codegen [Block]
word name args = do
  let mangled = rustifyInstantiated $ Instantiated name args
  isInstantiated <- liftA2 (<|>) (inDone mangled) (inTodo mangled)
  case isInstantiated of
    Just e -> callWord False mangled e
    _ -> do
      dict <- getDict
      case Map.lookup mangled dict of
        Just e -> callWord True mangled e
        _ -> do
          let unMangled = rustifyInstantiated $ Instantiated name []
          case Map.lookup unMangled dict of
            Just (Entry.WordEntry a b c d e (Just body)) ->
              liftIO (runMlatu $ Instantiate.term TypeEnv.empty body args)
                >>= ( \case
                        Right body' -> callWord True mangled (Entry.WordEntry a b c d e (Just body'))
                        Left _ -> error "Could not instantiate generic type"
                    )
            _ -> pure []

callWord :: Bool -> ByteString -> WordEntry -> Codegen [Block]
callWord b name e@(Entry.WordEntry _ _ _ _ _ (Just body)) = case decompose body of
  [New _ _ (ConstructorIndex 0) 0 NatLike] -> pure [PushNat "0"]
  [New _ _ (ConstructorIndex 1) 1 NatLike] -> pure [PushNat "stack.nat_succ()"]
  [New _ _ (ConstructorIndex 1) 1 ListLike] -> pure [PushList "Vec::new()"]
  [New _ _ (ConstructorIndex 1) 2 ListLike] ->
    pure [Unwrap "x", UnwrapList "mut xs", Custom "xs.insert(0, x);", PushList "xs"]
  [New _ _ (ConstructorIndex i) 0 NonSpecial] -> do
    io <- isOnline
    pure [PushAlgebraic (show i) ((if io then "Small" else "") <> "Vec::new()")]
  [New _ _ (ConstructorIndex i) 1 NonSpecial] -> do
    io <- isOnline
    pure [Let "v" ((if io then "small" else "") <> "vec![" <> get <> "];"), PushAlgebraic (show i) "v"]
  [New _ _ (ConstructorIndex i) size NonSpecial] -> do
    io <- isOnline
    pure
      [ Let "mut v" (vecBuilder size (if io then "smallvec" else "vec")),
        Custom "v.reverse();",
        PushAlgebraic (show i) "v"
      ]
  _ -> do
    when b $ modifyToDo $ Map.insert name e
    pure [Call name]

intrinsic :: Text -> Codegen [Block]
intrinsic = \case
  "call" ->
    pure
      [ UnwrapClosure "name" "new",
        Let "old" "closures",
        Let "closures" "&new",
        Call "name",
        Let "closures" "old"
      ]
  "abort" -> pure [UnwrapText "a", Custom "return Err(AbortCalled(a));"]
  "exit" -> pure [UnwrapNat "i", Custom "std::process::exit(i as i32);"]
  "drop" -> pure [Unwrap "_"]
  "swap" -> pure [Unwrap "a", Unwrap "b", Push_ "a", Push_ "b"]
  "dup" -> pure [Unwrap "a", Push_ "a.clone()", Push_ "a"]
  "cmp-char" -> (\c -> [UnwrapChar "a", UnwrapChar "b", c]) <$> cmp "a" "b"
  "cmp-string" -> (\c -> [UnwrapText "a", UnwrapText "b", c]) <$> cmp "a" "b"
  "show-nat" -> pure [UnwrapNat "i", PushText "format!(\"{}\", i)"]
  "read-nat" -> do
    s <- word (Global "some") []
    n <- word (Global "none") []
    pure
      [ UnwrapText "a",
        Custom
          ( "if let Some(a) = a.parse().ok().map(Nat) { stack.push(a); "
              <> toBS s
              <> "} else {"
              <> toBS n
              <> "}"
          )
      ]
  "string-concat" -> pure [UnwrapText "a", UnwrapText "mut b", Custom "b.push_str(&a);", PushText "b"]
  "string-from-list" ->
    pure
      [ UnwrapList "a",
        PushText "a.iter().filter_map(|e| if let Char(c) = e { Some(c) } else { None } ).collect::<String>()"
      ]
  "string-to-list" -> pure [UnwrapText "a", PushList "a.chars().map(Char).collect::<Vec<_>>()"]
  "write-stdout" ->
    pure
      [ UnwrapText "contents",
        Custom "use std::io::Write; std::io::stdout().write_all(contents.as_bytes()).map_err(|_| IOError)?;"
      ]
  "write-stderr" ->
    pure
      [ UnwrapText "contents",
        Custom
          "use std::io::Write; std::io::stderr().write_all(contents.as_bytes()).map_err(|_| IOError)?"
      ]
  "write-file" ->
    pure
      [ UnwrapText "filename",
        UnwrapText "contents",
        Let "mut file" "std::fs::File::create(filename).map_err(|_| IOError)?",
        Custom
          "use std::io::Write; file.write_all(contents.as_bytes()).map_err(|_| IOError)?;"
      ]
  "flush-stdout" -> pure [Custom "use std::io::Write; std::io::stdout().flush().map_err(|_| IOError)?;"]
  "flush-stderr" -> pure [Custom "use std::io::Write; std::io::stderr().flush().map_err(|_| IOError)?"]
  "flush-file" ->
    pure
      [ UnwrapText "filename",
        Let "mut file" "std::fs::File::create(filename).map_err(|_| IOError)?",
        Custom "use std::io::Write; file.flush().map_err(|_| IOError)?;"
      ]
  "read-line" ->
    pure
      [ Let "mut buffer" "String::new()",
        Custom "std::io::stdin().read_line(&mut buffer).map_err(|_| IOError)?;",
        PushText "buffer"
      ]
  "read-file" ->
    pure
      [ UnwrapText "filename",
        Let "mut file" "std::fs::File::open(filename).map_err(|_| IOError)?",
        Let "mut buffer" "String::new()",
        Custom "file.read_to_string(&mut buffer).map_err(|_| IOError)?;",
        PushText "buffer"
      ]
  "read-stdin" ->
    pure
      [ Let "mut buffer" "String::new()",
        Custom "use std::io::Read; std::io::stdin().read_to_string(&mut buffer).map_err(|_| IOError)?;",
        PushText "buffer"
      ]
  x -> error ("No such intrinsic: " <> show x)

cmp :: ByteString -> ByteString -> Codegen Block
cmp a b = do
  m <- word (Global "more") []
  l <- word (Global "less") []
  e <- word (Global "equal") []
  pure $
    MatchBlock
      (b <> ".cmp(&" <> a <> ")")
      [ ("std::cmp::Ordering::Greater", m),
        ("std::cmp::Ordering::Less", l),
        ("std::cmp::Ordering::Equal", e)
      ]

vecBuilder :: Int -> ByteString -> ByteString
vecBuilder 0 b = b <> "![]"
vecBuilder x b = b <> "![" <> go x <> "]"
  where
    go 1 = get
    go num = get <> ", " <> go (num - 1)

caseRs :: (Origin, GeneralName, Term Type) -> Codegen (Maybe (ByteString, [Block]))
caseRs (_, QualifiedName name, caseBody) = do
  dict <- getDict
  case Map.lookup (rustifyInstantiated (Instantiated name [])) dict of
    Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ _ (ConstructorIndex i) 0 NonSpecial] ->
          Just
            . ("Algebraic(" <> show i <> ", _)",)
            <$> termRs caseBody
        [New _ _ (ConstructorIndex i) 1 NonSpecial] ->
          Just
            . ("Algebraic(" <> show i <> ", mut fields)",)
            . (Push_ "*fields.swap_remove(0)" :)
            <$> termRs caseBody
        [New _ _ (ConstructorIndex i) _ NonSpecial] ->
          Just
            . ("Algebraic(" <> show i <> ", fields)",)
            . (Custom "for field in fields { stack.push(*field); } " :)
            <$> termRs caseBody
        [New _ _ (ConstructorIndex 0) 0 NatLike] ->
          Just
            . ("Nat(0)",)
            <$> termRs caseBody
        [New _ _ (ConstructorIndex 1) 1 NatLike] ->
          Just . ("Nat(a) if a > 0 ",)
            . (PushNat "a-1" :)
            <$> termRs caseBody
        [New _ _ (ConstructorIndex 0) 0 ListLike] ->
          Just . ("List(v) if v.is_empty()",) <$> termRs caseBody
        [New _ _ (ConstructorIndex 1) 2 ListLike] ->
          Just . ("List(mut v) if !v.is_empty() ",)
            . ([Let "x" "v.remove(0)", Push_ "x", PushList "v"] <>)
            <$> termRs caseBody
        _ -> pure Nothing
    _ -> pure Nothing
caseRs _ = pure Nothing

rustify :: ByteString -> ByteString
rustify txt =
  let string :: String = decodeUtf8 txt
      newString :: String =
        "m"
          <> concatMap
            (\c -> if isAlphaNum c then [c] else if c == '-' then "_" else printf "%x" (ord c))
            string
   in encodeUtf8 newString

rustifyQualified :: Qualified -> ByteString
rustifyQualified = rustify . show . printQualified

rustifyInstantiated :: Instantiated -> ByteString
rustifyInstantiated = rustify . show . printInstantiated
