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
  bs <- evalCodegen (untilM entryRs (Map.null <$> getToDo)) (one (firstKey, firstEntry), Map.empty) (Map.mapKeys rustifyInstantiated (view wordEntries dict))
  pure
    ( "#![allow(warnings)] extern crate smallvec; fn main() { match "
        <> firstKey
        <> "(&mut Stack::new(), &Vec::new()) {\
           \Err(AbortCalled(s)) => eprintln!(\"Abort called: {}\", s),\
           \Err(CompilerError) => eprintln!(\"Internal compiler error\"),\
           \Err(IOError) => eprintln!(\"IO error\"),\
           \Ok(()) => {},\
           \} }\
           \ type StackFn = fn(&mut Stack, &[Rep]) -> StackResult<()>;\
           \ type StackResult<T> = Result<T, Error>;\
           \ type AVec = SmallVec<[Box<Rep>; 2]>;\
           \ #[derive(Clone)] enum Rep { Closure(StackFn, Vec<Rep>), Algebraic(usize, AVec), Nat(usize), List(Vec<Rep>), Char(char), Text(String) } \
           \ #[derive(Clone)] enum Error { AbortCalled(String), CompilerError, IOError } \
           \ #[derive(Clone)] struct Stack { pub inner: Vec<Rep> }\
           \ use Rep::*; use Error::*; use smallvec::*;\
           \ impl Stack {\
           \ #[inline] fn new() -> Self { Self { inner: Vec::with_capacity(6), } }\
           \ #[inline] fn get(&mut self) -> StackResult<Rep> { if let Some(a) = self.inner.pop() { Ok(a) } else { Err(CompilerError) } }\
           \ #[inline] fn get_nat(&mut self) -> StackResult<usize> { if let Ok(Nat(a)) = self.get() { Ok(a) } else { Err(CompilerError) }} \
           \ #[inline] fn get_text(&mut self) -> StackResult<String> { if let Ok(Text(a)) = self.get() { Ok(a) } else { Err(CompilerError) }} \
           \ #[inline] fn get_closure(&mut self) -> StackResult<(StackFn, Vec<Rep>)> { if let Ok(Closure(a, b)) = self.get() { Ok((a, b)) } else { Err(CompilerError) }} \
           \ #[inline] fn get_char(&mut self) -> StackResult<char> { if let Ok(Char(a)) = self.get() { Ok(a) } else { Err(CompilerError) }} \
           \ #[inline] fn get_algebraic(&mut self) -> StackResult<(usize, AVec)> { if let Ok(Algebraic(a, b)) = self.get() { Ok((a, b)) } else { Err(CompilerError) }} \
           \ #[inline] fn get_list(&mut self) -> StackResult<Vec<Rep>> { if let Ok(List(a)) = self.get() { Ok(a) } else { Err(CompilerError) }} \
           \ #[inline] fn push(&mut self, n: Rep) { self.inner.push(n) } \
           \ #[inline] fn push_nat(&mut self, n: usize) { self.push(Nat(n)); } \
           \ #[inline] fn push_text(&mut self, n: String) { self.push(Text(n)); } \
           \ #[inline] fn push_closure(&mut self, a: StackFn, b: Vec<Rep>) { self.push(Closure(a,b)); } \
           \ #[inline] fn push_char(&mut self, n: char) { self.push(Char(n)); } \
           \ #[inline] fn push_algebraic(&mut self, a: usize, b: AVec) { self.push(Algebraic(a,b)); } \
           \ #[inline] fn push_list(&mut self, n: Vec<Rep>) { self.push(List(n)); } \
           \ #[inline] fn nat_pred(&mut self) { if let Some(Nat(n)) = self.inner.last_mut() { *n -= 1; } }\
           \ #[inline] fn nat_succ(&mut self) { if let Some(Nat(n)) = self.inner.last_mut() { *n += 1; } }\
           \}"
        <> ByteString.concat bs
    )

type WordMap = Map ByteString WordEntry

newtype Codegen a = Codegen (StateT (WordMap, WordMap) (ReaderT WordMap IO) a)
  deriving (Monad, Functor, Applicative, MonadState (WordMap, WordMap), MonadReader WordMap, MonadIO)

evalCodegen :: Codegen a -> (WordMap, WordMap) -> WordMap -> IO a
evalCodegen (Codegen c) initialState = runReaderT (evalStateT c initialState)

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

entryRs :: Codegen ByteString
entryRs =
  getToDo
    >>= ( ( \case
              Nothing -> pure ""
              Just ((i, e), newMap) ->
                (setToDo newMap >> modifyDone (Map.insert i e))
                  >> ( case e of
                         (Entry.WordEntry _ _ _ _ _ (Just body)) -> do
                           b <- termRs body
                           pure $ stackFn i b $ countLocal body
                         _ -> pure ""
                     )
          )
            . Map.minViewWithKey
        )

termRs :: Term Type -> Codegen ByteString
termRs x = goTerms $ decompose x
  where
    goTerms = \case
      [] -> pure ""
      (Push _ (Text x) _ : Word _ (QualifiedName (Global "extern")) _ _ : xs) -> do
        cg <- intrinsic x
        rest <- goTerms xs
        pure (cg <> rest)
      (Push _ (Name name) _ : NewClosure _ size _ : xs) ->
        ( ( case size of
              0 -> pushClosure (rustifyQualified name) "Vec::new()"
              _ ->
                letStmt "v" (vecBuilder size "vec")
                  <> pushClosure (rustifyQualified name) "v"
          )
            <>
        )
          <$> goTerms xs
      (Word _ (QualifiedName (Qualified _ "zero")) _ _ : xs) -> do
        let go :: Int -> [Term a] -> (Int, [Term a])
            go n ((Word _ (QualifiedName (Qualified _ "succ")) _ _) : xs) = go (n + 1) xs
            go n rest = (n, rest)
            (s, rest) = go 0 xs
        (pushNat (show s) <>) <$> goTerms rest
      (y : ys) -> do
        a <- goTerm y
        b <- goTerms ys
        pure $ a <> b
    goTerm = \case
      Group a -> termRs a
      Push _ (Character c) _ -> pure $ pushChar $ "'" <> show c <> "'"
      Push _ (Text txt) _ ->
        pure $
          pushText
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
      Push _ (Local (LocalIndex 0)) _ -> pure "stack.push(locals.last().unwrap().clone());"
      Push _ (Local (LocalIndex i)) _ -> pure $ "stack.push(locals[locals.len() - " <> show (1 + i) <> "].clone());"
      Push _ (Closed (ClosureIndex i)) _ -> pure $ "stack.push(closures[" <> show i <> "].clone());"
      Word _ (QualifiedName (Qualified _ "le")) [TypeConstructor _ "nat"] _ -> do
        t <- word (Global "true") []
        f <- word (Global "false") []
        pure $
          unwrapNat "a" <> unwrapNat "b" <> "if b <= a { "
            <> t
            <> " } else { "
            <> f
            <> "}"
      Word _ (QualifiedName (Qualified _ "cmp")) [TypeConstructor _ "nat"] _ ->
        ((unwrapNat "a" <> unwrapNat "b") <>) <$> cmp "a" "b"
      Word _ (QualifiedName (Qualified _ "pred")) _ _ -> pure "stack.nat_pred();"
      Word _ (QualifiedName (Qualified _ "+")) _ _ ->
        pure $ unwrapNat "a" <> unwrapNat "b" <> pushNat "a+b"
      Word _ (QualifiedName (Qualified _ "-")) _ _ ->
        pure $ unwrapNat "a" <> unwrapNat "b" <> pushNat "a-b"
      Word _ (QualifiedName (Qualified _ "*")) _ _ ->
        pure $ unwrapNat "a" <> unwrapNat "b" <> pushNat "a*b"
      Word _ (QualifiedName (Qualified _ "/")) _ _ ->
        pure $ unwrapNat "a" <> unwrapNat "b" <> pushNat "a/b"
      Word _ (QualifiedName name) args _ -> word name args
      Lambda _ _ _ body _ -> (\a -> "locals.push(stack.get()?);" <> a <> "locals.pop();") <$> termRs body
      Match _ cases els _ -> do
        cs <- traverse caseRs cases
        e <- case els of
          (Else body _) -> termRs body
          (DefaultElse _ _) -> word (Global "abort-now") []
        pure $ matchStmt "stack.get()" (catMaybes cs ++ [("_", e)])
      _ -> pure ""

inTodo :: ByteString -> Codegen (Maybe WordEntry)
inTodo bs = Map.lookup bs <$> getToDo

inDone :: ByteString -> Codegen (Maybe WordEntry)
inDone bs = Map.lookup bs <$> getDone

word :: Qualified -> [Type] -> Codegen ByteString
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
            _ -> pure ""

callWord :: Bool -> ByteString -> WordEntry -> Codegen ByteString
callWord b name e@(Entry.WordEntry _ _ _ _ _ (Just body)) = case decompose body of
  [New _ (ConstructorIndex 0) 0 NatLike _] -> pure $ pushNat "0"
  [New _ (ConstructorIndex 1) 1 NatLike _] -> pure $ pushNat "stack.nat_succ()"
  [New _ (ConstructorIndex 1) 1 ListLike _] -> pure $ pushList "Vec::new()"
  [New _ (ConstructorIndex 1) 2 ListLike _] ->
    pure $
      letStmt "x" "stack.get()?" <> unwrapList "mut xs" <> "xs.insert(0, x);" <> pushList "xs"
  [New _ (ConstructorIndex i) 0 NonSpecial _] -> pure $ pushAlgebraic (show i) "SmallVec::new()"
  [New _ (ConstructorIndex i) 1 NonSpecial _] ->
    pure $
      letStmt "v" "smallvec![stack.get()?]" <> pushAlgebraic (show i) "v"
  [New _ (ConstructorIndex i) size NonSpecial _] ->
    pure $
      letStmt "mut v" (vecBuilder size "smallvec") <> "v.reverse(); " <> pushAlgebraic (show i) "v"
  _ -> do
    when b $ modifyToDo $ Map.insert name e
    pure $ name <> "(stack, closures)?;"

intrinsic :: Text -> Codegen ByteString
intrinsic = \case
  "call" -> pure $ unwrapClosure "name" "new" <> "let old = closures; name(stack, &new)?; let closures = old;"
  "abort" -> pure $ unwrapText "a" <> "return Err(AbortCalled(a));"
  "exit" -> pure $ unwrapNat "i" <> "std::process::exit(i as i32);"
  "drop" -> pure $ letStmt "_" "stack.get()?"
  "swap" -> pure $ ifLetPop2 ("a", "b") "stack.push(a); stack.push(b);"
  "cmp-char" -> ((unwrapChar "a" <> unwrapChar "b") <>) <$> cmp "a" "b"
  "cmp-string" -> ((unwrapText "a" <> unwrapText "b") <>) <$> cmp "a" "b"
  "show-nat" -> pure $ unwrapNat "i" <> pushText "format!(\"{}\", i)"
  "read-nat" -> do
    s <- word (Global "some") []
    n <- word (Global "none") []
    pure $
      unwrapText "a"
        <> "if let Some(a) = a.parse().ok().map(Nat) { stack.push(a); "
        <> s
        <> "} else {"
        <> n
        <> "}"
  "string-concat" -> pure $ unwrapText "a" <> unwrapText "mut b" <> "b.push_str(&a);" <> pushText "b"
  "string-from-list" ->
    pure $
      unwrapList "a"
        <> pushText "a.iter().filter_map(|e| if let Char(c) = e { Some(c) } else { None } ).collect::<String>()"
  "string-to-list" -> pure $ unwrapText "a" <> pushList "a.chars().map(Char).collect::<Vec<_>>()"
  "write-stdout" ->
    pure $
      "use std::io::Write;"
        <> unwrapText "contents"
        <> "std::io::stdout().write_all(contents.as_bytes()).map_err(|_| IOError)?;"
  "write-stderr" ->
    pure $
      "use std::io::Write;"
        <> unwrapText "contents"
        <> "std::io::stderr().write_all(contents.as_bytes()).map_err(|_| IOError)?"
  "write-file" ->
    pure $
      "use std::io::Write;"
        <> unwrapText "filename"
        <> letStmt "mut file" "std::fs::File::create(filename).map_err(|_| IOError)?"
        <> unwrapText "contents"
        <> "file.write_all(contents.as_bytes()).map_err(|_| IOError)?;"
  "flush-stdout" -> pure $ "use std::io::Write;" <> "std::io::stdout().flush().map_err(|_| IOError)?;"
  "flush-stderr" -> pure $ "use std::io::Write;" <> "std::io::stderr().flush().map_err(|_| IOError)?"
  "flush-file" ->
    pure $
      "use std::io::Write;"
        <> unwrapText "filename"
        <> letStmt "mut file" "std::fs::File::create(filename).map_err(|_| IOError)?"
        <> "file.flush().map_err(|_| IOError)?;"
  "read-line" ->
    pure $
      letStmt "mut buffer" "String::new()"
        <> "std::io::stdin().read_line(&mut buffer).map_err(|_| IOError)?;"
        <> pushText "buffer"
  "read-file" ->
    pure $
      "use std::io::Read;"
        <> letStmt "mut buffer" "String::new()"
        <> unwrapText "filename"
        <> letStmt "mut file" "std::fs::File::open(filename).map_err(|_| IOError)?"
        <> "file.read_to_string(&mut buffer).map_err(|_| IOError)?;"
        <> pushText "buffer"
  "read-stdin" ->
    pure $
      "use std::io::Read;"
        <> letStmt "mut buffer" "String::new()"
        <> "std::io::stdin().read_to_string(&mut buffer).map_err(|_| IOError)?;"
        <> pushText "buffer"
  x -> error ("No such intrinsic: " <> show x)

cmp :: ByteString -> ByteString -> Codegen ByteString
cmp a b = do
  m <- word (Global "more") []
  l <- word (Global "less") []
  e <- word (Global "equal") []
  pure $
    matchStmt
      (b <> ".cmp(&" <> a <> ")")
      [ ("std::cmp::Ordering::Greater", m),
        ("std::cmp::Ordering::Less", l),
        ("std::cmp::Ordering::Equal", e)
      ]

vecBuilder :: Int -> ByteString -> ByteString
vecBuilder 0 b = b <> "![]"
vecBuilder x b = b <> "![" <> go x <> "]"
  where
    go 1 = "stack.get()?"
    go num = "stack.get()?, " <> go (num - 1)

caseRs :: Case Type -> Codegen (Maybe (ByteString, ByteString))
caseRs (Case (QualifiedName name) caseBody _) = do
  dict <- ask
  case Map.lookup (rustifyInstantiated (Instantiated name [])) dict of
    Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ (ConstructorIndex i) 0 NonSpecial _] ->
          Just
            . ("Ok(Algebraic(" <> show i <> ", _))",)
            <$> termRs caseBody
        [New _ (ConstructorIndex i) 1 NonSpecial _] ->
          Just
            . ("Ok(Algebraic(" <> show i <> ", mut fields))",)
            . ("stack.push(*fields.swap_remove(0));" <>)
            <$> termRs caseBody
        [New _ (ConstructorIndex i) _ NonSpecial _] ->
          Just
            . ("Ok(Algebraic(" <> show i <> ", fields))",)
            . ("for field in fields { stack.push(*field); } " <>)
            <$> termRs caseBody
        [New _ (ConstructorIndex 0) 0 NatLike _] ->
          Just
            . ("Ok(Nat(0))",)
            <$> termRs caseBody
        [New _ (ConstructorIndex 1) 1 NatLike _] ->
          Just . ("Ok(Nat(a)) if a > 0 ",)
            . (pushNat "a-1" <>)
            <$> termRs caseBody
        [New _ (ConstructorIndex 0) 0 ListLike _] ->
          Just . ("Ok(List(v)) if v.is_empty()",) <$> termRs caseBody
        [New _ (ConstructorIndex 1) 2 ListLike _] ->
          Just . ("Ok(List(mut v)) if !v.is_empty() ",)
            . ((letStmt "x" "v.remove(0)" <> "stack.push(x);" <> pushList "v") <>)
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

unwrapNat :: ByteString -> ByteString
unwrapNat a = letStmt a "stack.get_nat()?"

pushNat :: ByteString -> ByteString
pushNat a = "stack.push_nat(" <> a <> ");"

unwrapText :: ByteString -> ByteString
unwrapText a = letStmt a "stack.get_text()?"

pushText :: ByteString -> ByteString
pushText a = "stack.push_text(" <> a <> ");"

unwrapClosure :: ByteString -> ByteString -> ByteString
unwrapClosure a b = letStmt ("(" <> a <> "," <> b <> ")") "stack.get_closure()?"

pushClosure :: ByteString -> ByteString -> ByteString
pushClosure a b = "stack.push_closure(" <> a <> "," <> b <> ");"

unwrapChar :: ByteString -> ByteString
unwrapChar a = letStmt a "stack.get_char()?"

pushChar :: ByteString -> ByteString
pushChar a = "stack.push_char(" <> a <> ");"

pushAlgebraic :: ByteString -> ByteString -> ByteString
pushAlgebraic a b = "stack.push_algebraic(" <> a <> "," <> b <> ");"

unwrapList :: ByteString -> ByteString
unwrapList a = letStmt a "stack.get_list()?"

pushList :: ByteString -> ByteString
pushList a = "stack.push_list(" <> a <> ");"

stackFn :: ByteString -> ByteString -> Int -> ByteString
stackFn name body count =
  " #[inline]  fn "
    <> name
    <> "(stack: &mut Stack, closures: &[Rep]) -> StackResult<()> { "
    <> ( case count of
           0 -> ""
           n -> letStmt "mut locals: Vec<Rep>" ("Vec::with_capacity(" <> show n <> ")")
       )
    <> body
    <> " Ok(()) }"

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

ifLetPop :: ByteString -> ByteString -> ByteString
ifLetPop binding body =
  "if let Ok(" <> binding <> ") = stack.get() { "
    <> body
    <> " } else { return Err(AbortCalled(\"Inexhaustive pattern match\".to_owned())); }"

ifLetPop2 :: (ByteString, ByteString) -> ByteString -> ByteString
ifLetPop2 (b1, b2) body = ifLetPop b1 (ifLetPop b2 body)

matchStmt :: ByteString -> [(ByteString, ByteString)] -> ByteString
matchStmt target cases = "match " <> target <> " { " <> ByteString.concat ((\(binding, block) -> binding <> " => {" <> block <> "},") <$> cases) <> " };"

letStmt :: ByteString -> ByteString -> ByteString
letStmt binding expression = "let " <> binding <> " = " <> expression <> ";"
