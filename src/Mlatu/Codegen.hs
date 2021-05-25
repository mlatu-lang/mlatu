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
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Instantiated (Instantiated (..))
import Mlatu.Instantiated qualified as Instantiated
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name (ClosureIndex (..), ConstructorIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Unqualified (..))
import Mlatu.Pretty (printInstantiated, printQualified)
import Mlatu.Term (Case (..), Else (..), Specialness (..), Term (..), Value (..), decompose)
import Mlatu.Type (Type (..))
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Vocabulary
import Optics
import Relude hiding (Compose, Type)
import Text.Printf (printf)

instance IsString a => IsString (Codegen a) where
  fromString = pure . fromString

generate :: Dictionary -> Maybe Qualified -> IO ByteString
generate dict mMain = do
  let firstKey = maybe "mmain" rustifyQualified mMain
  let firstEntry = case Dictionary.lookupWord (Instantiated (fromMaybe mainName mMain) []) dict of
        Just e -> e
        Nothing -> error "Could not find main entry"
  bs <- evalCodegen (untilM entryRs (Map.null <$> getToDo)) (one (firstKey, firstEntry), Map.empty, False) (Map.mapKeys rustifyInstantiated (view wordEntries dict))
  pure
    ( "#![allow(warnings)] extern crate smallvec; \
      \ macro_rules! get { ($name:ident) => { $name.pop().ok_or(CompilerError)? } } \
      \ macro_rules! try_nat { ($e:expr) => { if let Nat(a) = $e { a } else { return Err(CompilerError); } }; } \
      \ macro_rules! try_list { ($e:expr) => { if let List(a) = $e { a } else { return Err(CompilerError); } }; } \
      \ macro_rules! try_char { ($e:expr) => { if let Char(a) = $e { a } else { return Err(CompilerError); } }; } \
      \ macro_rules! try_text { ($e:expr) => { if let Text(a) = $e { a } else { return Err(CompilerError); } }; } \
      \ macro_rules! try_algebraic { ($e:expr) => { if let Algebraic(a,b) = $e { (a,b) } else { return Err(CompilerError); } }; } \
      \ macro_rules! try_closure { ($e:expr) => { if let Closure(a,b) = $e { (a,b) } else { return Err(CompilerError); } }; } \
      \ fn main() { match "
        <> firstKey
        <> "(&mut Vec::with_capacity(6), &Vec::new()) {\
           \Err(AbortCalled(s)) => eprintln!(\"Abort called: {}\", s),\
           \Err(CompilerError) => eprintln!(\"Internal compiler error\"),\
           \Err(IOError) => eprintln!(\"IO error\"),\
           \Ok(()) => {},\
           \} }\
           \ type StackFn = fn(&mut Vec<Rep>, &[Rep]) -> StackResult<()>;\
           \ type StackResult<T> = Result<T, Error>;\
           \ type AVec = SmallVec<[Box<Rep>; 2]>;\
           \ #[derive(Clone)] enum Rep { Closure(StackFn, Vec<Rep>), Algebraic(usize, AVec), Nat(usize), List(Vec<Rep>), Char(char), Text(String) } \
           \ #[derive(Clone)] enum Error { AbortCalled(String), CompilerError, IOError } \
           \ use Rep::*; use Error::*; use smallvec::*; "
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
  | Custom ByteString
  deriving (Ord, Eq, Show)

letStmt :: ByteString -> ByteString -> ByteString
letStmt a b = "let " <> a <> " = " <> b <> ";"

couple :: ByteString -> ByteString -> ByteString
couple a b = "(" <> a <> "," <> b <> ")"

toBS :: [Block] -> ByteString
toBS = go
  where
    go [] = ""
    go (Push_ a : Unwrap b : rest) = letStmt b a <> toBS rest
    go (PushNat a : UnwrapNat b : rest) = letStmt b a <> toBS rest
    go (Push_ a : UnwrapNat b : rest) = letStmt b ("try_nat!(" <> a <> ")") <> toBS rest
    go (PushNat a : Unwrap b : rest) = letStmt b ("Nat(" <> a <> ")") <> toBS rest
    go (PushList a : UnwrapList b : rest) = letStmt b a <> toBS rest
    go (Push_ a : UnwrapList b : rest) = letStmt b ("try_list!(" <> a <> ")") <> toBS rest
    go (PushList a : Unwrap b : rest) = letStmt b ("List(" <> a <> ")") <> toBS rest
    go (PushChar a : UnwrapChar b : rest) = letStmt b a <> toBS rest
    go (Push_ a : UnwrapChar b : rest) = letStmt b ("try_char!(" <> a <> ")") <> toBS rest
    go (PushChar a : Unwrap b : rest) = letStmt b ("Char(" <> a <> ")") <> toBS rest
    go (PushText a : UnwrapText b : rest) = letStmt b a <> toBS rest
    go (Push_ a : UnwrapText b : rest) = letStmt b ("try_text!(" <> a <> ")") <> toBS rest
    go (PushText a : Unwrap b : rest) = letStmt b ("Text(" <> a <> ")") <> toBS rest
    go (PushAlgebraic a1 a2 : UnwrapAlgebraic b1 b2 : rest) = letStmt b1 a1 <> letStmt b2 a2 <> toBS rest
    go (Push_ a : UnwrapAlgebraic b1 b2 : rest) = letStmt (couple b1 b2) ("try_algebraic!(" <> a <> ")") <> toBS rest
    go (PushAlgebraic a1 a2 : Unwrap b : rest) = letStmt b ("Algebraic" <> couple a1 a2) <> toBS rest
    go (PushClosure a1 a2 : UnwrapClosure b1 b2 : rest) = letStmt b1 a1 <> letStmt b2 a2 <> toBS rest
    go (Push_ a : UnwrapClosure b1 b2 : rest) = letStmt (couple b1 b2) ("try_closure!(" <> a <> ")") <> toBS rest
    go (PushClosure a1 a2 : Unwrap b : rest) = letStmt b ("Closure" <> couple a1 a2) <> toBS rest
    go (FunBlock name body : rest) =
      "#[inline] fn " <> name
        <> "(stack: &mut Vec<Rep>, closures: &[Rep]) -> StackResult<()> { "
        <> toBS body
        <> " Ok(()) } "
        <> toBS rest
    go (UnwrapNat name : rest) = letStmt name "try_nat!(get!(stack))" <> toBS rest
    go (UnwrapChar name : rest) = letStmt name "try_char!(get!(stack))" <> toBS rest
    go (UnwrapText name : rest) = letStmt name "try_text!(get!(stack))" <> toBS rest
    go (UnwrapList name : rest) = letStmt name "try_list!(get!(stack))" <> toBS rest
    go (UnwrapClosure name1 name2 : rest) = letStmt (couple name1 name2) "try_closure!(get!(stack))" <> toBS rest
    go (UnwrapAlgebraic name1 name2 : rest) = letStmt (couple name1 name2) "try_algebraic!(get!(stack))" <> toBS rest
    go (Unwrap name : rest) = letStmt name "get!(stack)" <> toBS rest
    go (PushNat body : rest) = "stack.push(Nat(" <> body <> "));" <> toBS rest
    go (PushChar body : rest) = "stack.push(Char(" <> body <> "));" <> toBS rest
    go (PushText body : rest) = "stack.push(Text(" <> body <> "));" <> toBS rest
    go (PushList body : rest) = "stack.push(List(" <> body <> "));" <> toBS rest
    go (PushClosure body1 body2 : rest) =
      "stack.push(Closure" <> couple body1 body2 <> ");" <> toBS rest
    go (PushAlgebraic body1 body2 : rest) =
      "stack.push(Algebraic" <> couple body1 body2 <> ");" <> toBS rest
    go (Push_ body : rest) = "stack.push(" <> body <> ");" <> toBS rest
    go (MatchBlock x xs : rest) =
      "match " <> x <> " { "
        <> ByteString.concat ((\(binding, block) -> binding <> " => {" <> toBS block <> "},") <$> xs)
        <> " };"
        <> toBS rest
    go (Call name : rest) = name <> "(stack, closures)?;" <> toBS rest
    go (Custom body : rest) = body <> toBS rest

newtype Codegen a = Codegen (StateT (WordMap, WordMap, Bool) (ReaderT WordMap IO) a)
  deriving (Monad, Functor, Applicative, MonadState (WordMap, WordMap, Bool), MonadReader WordMap, MonadIO)

evalCodegen :: Codegen a -> (WordMap, WordMap, Bool) -> WordMap -> IO a
evalCodegen (Codegen c) initialState = runReaderT (evalStateT c initialState)

getLocalState :: Codegen Bool
getLocalState = use _3

setLocalState :: Bool -> Codegen ()
setLocalState = assign _3

getToDo :: Codegen WordMap
getToDo = use _1

setToDo :: WordMap -> Codegen ()
setToDo = assign _1

modifyToDo :: (WordMap -> WordMap) -> Codegen ()
modifyToDo = modifying _1

getDone :: Codegen WordMap
getDone = use _2

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
                           let n = countLocal body
                           if n == 1
                             then setLocalState True
                             else setLocalState False
                           b <- termRs body
                           pure [stackFn i b n]
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
      (Push _ (Text x) _ : Word _ (QualifiedName (Global "extern")) _ _ : xs) ->
        liftA2 (<>) (intrinsic x) (goTerms xs)
      (Push _ (Name name) _ : NewClosure _ size _ : xs) ->
        ( ( case size of
              0 -> [PushClosure (rustifyQualified name) "Vec::new()"]
              _ ->
                [ Custom ("let v = " <> vecBuilder size "vec" <> ";"),
                  PushClosure (rustifyQualified name) "v"
                ]
          )
            <>
        )
          <$> goTerms xs
      (Word _ (QualifiedName (Qualified _ "zero")) _ _ : xs) -> do
        let go :: Int -> [Term a] -> (Int, [Term a])
            go n ((Word _ (QualifiedName (Qualified _ "succ")) _ _) : xs) = go (n + 1) xs
            go n rest = (n, rest)
            (s, rest) = go 0 xs
        (PushNat (show s) :) <$> goTerms rest
      (y : ys) -> liftA2 (<>) (goTerm y) (goTerms ys)
    goTerm = \case
      Group a -> termRs a
      Push _ (Character c) _ -> pure [PushChar $ "'" <> show c <> "'"]
      Push _ (Text txt) _ ->
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
      Push _ (Local (LocalIndex i)) _ -> do
        ls <- getLocalState
        pure
          [ Push_
              ( if ls
                  then "local1.clone()"
                  else case i of
                    0 -> "locals.last().unwrap().clone()"
                    _ -> "locals[locals.len() - " <> show (1 + i) <> "].clone()"
              )
          ]
      Push _ (Closed (ClosureIndex i)) _ -> pure [Push_ ("closures[" <> show i <> "].clone()")]
      Word _ (QualifiedName (Qualified _ "cmp")) [TypeConstructor _ "nat"] _ ->
        (\c -> [UnwrapNat "a", UnwrapNat "b", c]) <$> cmp "a" "b"
      Word _ (QualifiedName (Qualified _ "pred")) _ _ ->
        pure [UnwrapNat "a", PushNat "a-1"]
      Word _ (QualifiedName (Qualified _ "+")) _ _ ->
        pure [UnwrapNat "a", UnwrapNat "b", PushNat "a+b"]
      Word _ (QualifiedName (Qualified _ "-")) _ _ ->
        pure [UnwrapNat "a", UnwrapNat "b", PushNat "a-b"]
      Word _ (QualifiedName (Qualified _ "*")) _ _ ->
        pure [UnwrapNat "a", UnwrapNat "b", PushNat "a*b"]
      Word _ (QualifiedName (Qualified _ "/")) _ _ ->
        pure [UnwrapNat "a", UnwrapNat "b", PushNat "a/b"]
      Word _ (QualifiedName name) args _ -> word name args
      Lambda _ _ _ body _ -> do
        b <- termRs body
        ls <- getLocalState
        pure $
          if ls
            then Custom "let local1 = get!(stack);" : b
            else Custom "locals.push(get!(stack));" : b <> [Custom "locals.pop();"]
      Match _ cases els _ -> do
        cs <- traverse caseRs cases
        e <- case els of
          (Else body _) -> termRs body
          (DefaultElse _ _) -> word (Global "abort-now") []
        pure [MatchBlock "get!(stack)" (catMaybes cs ++ [("_", e)])]
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
      dict <- ask
      case Map.lookup mangled dict of
        Just e -> callWord True mangled e
        _ -> do
          let unMangled = rustifyInstantiated $ Instantiated name []
          case Map.lookup unMangled dict of
            Just (Entry.WordEntry a b c d e (Just body)) ->
              liftIO (runMlatuExceptT $ Instantiate.term TypeEnv.empty body args)
                >>= ( \case
                        Right body' -> callWord True mangled (Entry.WordEntry a b c d e (Just body'))
                        Left _ -> error "Could not instantiate generic type"
                    )
            _ -> pure []

callWord :: Bool -> ByteString -> WordEntry -> Codegen [Block]
callWord b name e@(Entry.WordEntry _ _ _ _ _ (Just body)) = case decompose body of
  [New _ (ConstructorIndex 0) 0 NatLike _] -> pure [PushNat "0"]
  [New _ (ConstructorIndex 1) 1 NatLike _] -> pure [PushNat "stack.nat_succ()"]
  [New _ (ConstructorIndex 1) 1 ListLike _] -> pure [PushList "Vec::new()"]
  [New _ (ConstructorIndex 1) 2 ListLike _] ->
    pure [Custom "let x = get!(stack);", UnwrapList "mut xs", Custom "xs.insert(0, x);", PushList "xs"]
  [New _ (ConstructorIndex i) 0 NonSpecial _] -> pure [PushAlgebraic (show i) "SmallVec::new()"]
  [New _ (ConstructorIndex i) 1 NonSpecial _] ->
    pure [Custom "let v = smallvec![get!(stack)];", PushAlgebraic (show i) "v"]
  [New _ (ConstructorIndex i) size NonSpecial _] ->
    pure [Custom ("let mut v = " <> vecBuilder size "smallvec" <> "; v.reverse(); "), PushAlgebraic (show i) "v"]
  _ -> do
    when b $ modifyToDo $ Map.insert name e
    pure [Call name]

intrinsic :: Text -> Codegen [Block]
intrinsic = \case
  "call" ->
    pure
      [ UnwrapClosure "name" "new",
        Custom "let old = closures; let closures = &new;",
        Call "name",
        Custom "let closures = old;"
      ]
  "abort" -> pure [UnwrapText "a", Custom "return Err(AbortCalled(a));"]
  "exit" -> pure [UnwrapNat "i", Custom "std::process::exit(i as i32);"]
  "drop" -> pure [Custom "let _ = get!(stack)"]
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
        Custom
          "use std::io::Write; let mut file = std::fs::File::create(filename).map_err(|_| IOError)?; file.write_all(contents.as_bytes()).map_err(|_| IOError)?;"
      ]
  "flush-stdout" -> pure [Custom "use std::io::Write; std::io::stdout().flush().map_err(|_| IOError)?;"]
  "flush-stderr" -> pure [Custom "use std::io::Write; std::io::stderr().flush().map_err(|_| IOError)?"]
  "flush-file" ->
    pure
      [ UnwrapText "filename",
        Custom "let mut file = std::fs::File::create(filename).map_err(|_| IOError)?; use std::io::Write; file.flush().map_err(|_| IOError)?;"
      ]
  "read-line" ->
    pure
      [ Custom "let mut buffer = String::new(); std::io::stdin().read_line(&mut buffer).map_err(|_| IOError)?;",
        PushText "buffer"
      ]
  "read-file" ->
    pure
      [ UnwrapText "filename",
        Custom "let mut file = std::fs::File::open(filename).map_err(|_| IOError)? use std::io::Read; let mut buffer = String::new(); file.read_to_string(&mut buffer).map_err(|_| IOError)?;",
        PushText "buffer"
      ]
  "read-stdin" ->
    pure
      [ Custom "let mut Buffer = String::new(); use std::io::Read; std::io::stdin().read_to_string(&mut buffer).map_err(|_| IOError)?;",
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
    go 1 = "get!(stack)"
    go num = "get!(stack), " <> go (num - 1)

caseRs :: Case Type -> Codegen (Maybe (ByteString, [Block]))
caseRs (Case (QualifiedName name) caseBody _) = do
  dict <- ask
  case Map.lookup (rustifyInstantiated (Instantiated name [])) dict of
    Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ (ConstructorIndex i) 0 NonSpecial _] ->
          Just
            . ("Algebraic(" <> show i <> ", _)",)
            <$> termRs caseBody
        [New _ (ConstructorIndex i) 1 NonSpecial _] ->
          Just
            . ("Algebraic(" <> show i <> ", mut fields)",)
            . (Push_ "*fields.swap_remove(0)" :)
            <$> termRs caseBody
        [New _ (ConstructorIndex i) _ NonSpecial _] ->
          Just
            . ("Algebraic(" <> show i <> ", fields)",)
            . (Custom "for field in fields { stack.push(*field); } " :)
            <$> termRs caseBody
        [New _ (ConstructorIndex 0) 0 NatLike _] ->
          Just
            . ("Nat(0)",)
            <$> termRs caseBody
        [New _ (ConstructorIndex 1) 1 NatLike _] ->
          Just . ("Nat(a) if a > 0 ",)
            . (PushNat "a-1" :)
            <$> termRs caseBody
        [New _ (ConstructorIndex 0) 0 ListLike _] ->
          Just . ("List(v) if v.is_empty()",) <$> termRs caseBody
        [New _ (ConstructorIndex 1) 2 ListLike _] ->
          Just . ("List(mut v) if !v.is_empty() ",)
            . ([Custom "let x = v.remove(0);", Push_ "x", PushList "v"] <>)
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

stackFn :: ByteString -> [Block] -> Int -> Block
stackFn name body count =
  FunBlock
    name
    ( if count <= 1
        then body
        else Custom ("let mut locals: Vec<Rep> = Vec::with_capacity(" <> show count <> ");") : body
    )

countLocal :: Term a -> Int
countLocal t =
  sum
    ( ( \case
          Lambda _ _ _ body _ -> countLocal body + 1
          Match _ cases e _ ->
            sum ((\case Case _ t _ -> countLocal t) <$> cases)
              + ( case e of
                    DefaultElse {} -> 0
                    Else t _ -> countLocal t
                )
          _ -> 0
      )
        <$> decompose t
    )
