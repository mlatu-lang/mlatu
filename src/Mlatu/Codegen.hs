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
  bs <- evalCodegen (untilM entryRs (Map.null <$> getToDo)) (Map.mapKeys rustifyInstantiated (view wordEntries dict), Map.empty) dict
  pure
    ( "#![allow(non_snake_case, dead_code, unused_mut, unused_variables, unused_assignments, unreachable_code)]"
        <> ( "fn main() { "
               <> maybe "mmain" rustifyQualified mMain
               <> "(&mut Stack::new(), &Vec::new()); }"
           )
        <> "type StackFn = fn(&mut Stack, &Vec<Rep>);"
        <> "#[derive(Clone)] enum Rep { Closure(StackFn, Vec<Rep>), Algebraic(usize, Vec<Rep>), Nat(usize), List(Vec<Rep>), Char(char), Text(String) } "
        <> "#[derive(Clone)] struct Stack { inner: Vec<Rep> }"
        <> "use Rep::*; "
        <> "impl Stack {"
        <> ( "fn new() -> Self { Self { inner: Vec::new(), } } "
               <> "fn get(&mut self) -> Option<Rep> { self.inner.pop() } "
               <> "fn get_nat(&mut self) -> Option<usize> { if let Some(Nat(a)) = self.inner.pop() { Some(a) } else { None }} "
               <> "fn get_text(&mut self) -> Option<String> { if let Some(Text(a)) = self.inner.pop() { Some(a) } else { None }} "
               <> "fn get_closure(&mut self) -> Option<(StackFn, Vec<Rep>)> { if let Some(Closure(a, b)) = self.inner.pop() { Some((a, b)) } else { None }} "
               <> "fn get_char(&mut self) -> Option<char> { if let Some(Char(a)) = self.inner.pop() { Some(a) } else { None }} "
               <> "fn get_algebraic(&mut self) -> Option<(usize, Vec<Rep>)> { if let Some(Algebraic(a, b)) = self.inner.pop() { Some((a, b)) } else { None }} "
               <> "fn get_list(&mut self) -> Option<Vec<Rep>> { if let Some(List(a)) = self.inner.pop() { Some(a) } else { None }} "
               <> "fn push(&mut self, n: Rep) { self.inner.push(n) } "
               <> "fn push_nat(&mut self, n: usize) { self.inner.push(Nat(n)); } "
               <> "fn push_text(&mut self, n: String) { self.inner.push(Text(n)); } "
               <> "fn push_closure(&mut self, a: StackFn, b: Vec<Rep>) { self.inner.push(Closure(a,b)); } "
               <> "fn push_char(&mut self, n: char) { self.inner.push(Char(n)); } "
               <> "fn push_algebraic(&mut self, a: usize, b: Vec<Rep>) { self.inner.push(Algebraic(a,b)); } "
               <> "fn push_list(&mut self, n: Vec<Rep>) { self.inner.push(List(n)); } "
               <> "fn nat_pred(&mut self) { if let Some(Nat(n)) = self.inner.last_mut() { *n -= 1; } }"
               <> "fn nat_succ(&mut self) { if let Some(Nat(n)) = self.inner.last_mut() { *n += 1; } }"
           )
        <> "}"
        <> ByteString.concat bs
    )

type WordMap = Map ByteString WordEntry

newtype Codegen a = Codegen (StateT (WordMap, WordMap) (ReaderT Dictionary IO) a)
  deriving (Monad, Functor, Applicative, MonadState (WordMap, WordMap), MonadReader Dictionary, MonadIO)

evalCodegen :: Codegen a -> (WordMap, WordMap) -> Dictionary -> IO a
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
                           pure $ stackFn i b $ containsLocals body
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
                letStmt "v" (vecBuilder size)
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
      Lambda _ _ _ body _ -> (\a -> "locals.push(stack.get().unwrap());" <> a <> "locals.pop();") <$> termRs body
      Match _ _ cases els _ -> do
        cs <- traverse caseRs cases
        e <- case els of
          (Else body _) -> termRs body
          (DefaultElse _ _) -> word (Global "abort-now") []
        pure $ matchStmt "stack.get()" (catMaybes cs ++ [("_", e)])
      _ -> pure ""

constructor :: Int -> Int -> Specialness -> ByteString
constructor 0 0 NatLike = pushNat "0"
constructor 1 1 NatLike = "stack.nat_succ();"
constructor 0 0 ListLike = pushList "Vec::new()"
constructor 1 2 ListLike = letStmt "x" "stack.get().unwrap()" <> unwrapList "mut xs" <> "xs.insert(0, x);" <> pushList "xs"
constructor i 0 NonSpecial = pushAlgebraic (show i) "Vec::new()"
constructor i 1 NonSpecial = letStmt "v" "vec![stack.get().unwrap()]" <> pushAlgebraic (show i) "v"
constructor i size NonSpecial = letStmt "mut v" (vecBuilder size) <> "v.reverse(); " <> pushAlgebraic (show i) "v"

word :: Qualified -> [Type] -> Codegen ByteString
word name args = do
  let mangled = rustifyInstantiated $ Instantiated name args
  isInstantiated <- uncurry ((. Map.lookup mangled) . (<|>) . Map.lookup mangled) <$> get
  case isInstantiated of
    Just (Entry.WordEntry _ _ _ _ _ (Just body)) -> case decompose body of
      [New _ (ConstructorIndex i) size b _] -> pure $ constructor i size b
      _ -> pure $ mangled <> "(stack, closures);"
    _ -> do
      let unMangled = rustifyInstantiated $ Instantiated name []
      isUninstantiated <- uncurry ((. Map.lookup unMangled) . (<|>) . Map.lookup unMangled) <$> get
      case isUninstantiated of
        Just (Entry.WordEntry a b c d e (Just body)) ->
          liftIO (runMlatuExceptT $ Instantiate.term TypeEnv.empty body args)
            >>= ( \case
                    Right body' -> do
                      modifyToDo $ Map.insert mangled (Entry.WordEntry a b c d e (Just body'))
                      case decompose body' of
                        [New _ (ConstructorIndex i) size b _] -> pure $ constructor i size b
                        _ -> pure $ mangled <> "(stack, closures);"
                    Left _ -> error "Could not instantiate generic type"
                )
        _ -> pure ""

intrinsic :: Text -> Codegen ByteString
intrinsic = \case
  "call" -> pure $ unwrapClosure "name" "new" <> "let old = closures; name(stack, &new); let closures = old;"
  "abort" -> pure $ unwrapText "a" <> "panic!(\"Execution failure: {}\", a);"
  "exit" -> pure $ unwrapNat "i" <> "std::process::exit(i as i32);"
  "drop" -> pure $ letStmt "_" "stack.get().unwrap();"
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
  "string-from-list" -> pure $ unwrapList "a" <> pushText "a.iter().filter_map(|e| if let Char(c) = e { Some(c) } else { None } ).collect::<String>()"
  "string-to-list" -> pure $ unwrapText "a" <> pushList "a.chars().map(Char).collect::<Vec<_>>()"
  "print" -> pure $ unwrapText "a" <> "print!(\"{}\", a);"
  "get-line" -> pure $ letStmt "mut buf" "String::new()" <> "std::io::stdin().read_line(&mut buf).unwrap();" <> pushText "buf"
  "flush-stdout" -> pure "use std::io::Write; std::io::stdout().flush().unwrap();"
  "read-file" -> pure $ unwrapText "a" <> pushText "std::fs::read_to_string(a).unwrap()"
  "write-file" ->
    pure $
      unwrapText "a" <> unwrapText "b" <> "use std::io::Write;"
        <> letStmt "mut file" "std::fs::File::create(b).unwrap()"
        <> "file.write_all(a.as_bytes()).unwrap();"
  "append-file" ->
    pure $
      unwrapText "a" <> unwrapText "b" <> "use std::io::Write;"
        <> letStmt "mut file" "std::fs::File::open(b).unwrap()"
        <> "file.write_all(a.as_bytes()).unwrap();"
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

vecBuilder :: Int -> ByteString
vecBuilder 0 = "Vec::new()"
vecBuilder x = "vec![" <> go x <> "]"
  where
    go 1 = "stack.get().unwrap()"
    go num = "stack.get().unwrap(), " <> go (num - 1)

caseRs :: Case Type -> Codegen (Maybe (ByteString, ByteString))
caseRs (Case (QualifiedName name) caseBody _) = do
  dict <- ask
  case Dictionary.lookupWord (Instantiated name []) dict of
    Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ (ConstructorIndex i) 0 NonSpecial _] ->
          Just
            . ("Some(Algebraic(" <> show i <> ", _))",)
            <$> termRs caseBody
        [New _ (ConstructorIndex i) 1 NonSpecial _] ->
          Just
            . ("Some(Algebraic(" <> show i <> ", mut fields))",)
            . ("stack.push(fields.swap_remove(0));" <>)
            <$> termRs caseBody
        [New _ (ConstructorIndex i) _ NonSpecial _] ->
          Just
            . ("Some(Algebraic(" <> show i <> ", fields))",)
            . ("for field in fields { stack.push(field); } " <>)
            <$> termRs caseBody
        [New _ (ConstructorIndex 0) 0 NatLike _] ->
          Just
            . ("Some(Nat(0))",)
            <$> termRs caseBody
        [New _ (ConstructorIndex 1) 1 NatLike _] ->
          Just . ("Some(Nat(a)) if a > 0 ",)
            . (pushNat "a-1" <>)
            <$> termRs caseBody
        [New _ (ConstructorIndex 0) 0 ListLike _] ->
          Just . ("Some(List(v)) if v.is_empty()",) <$> termRs caseBody
        [New _ (ConstructorIndex 1) 2 ListLike _] ->
          Just . ("Some(List(mut v)) if !v.is_empty() ",)
            . ((letStmt "x" "v.remove(0);" <> "stack.push(x);" <> pushList "v") <>)
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
unwrapNat a = "let " <> a <> " = stack.get_nat().unwrap();"

pushNat :: ByteString -> ByteString
pushNat a = "stack.push_nat(" <> a <> ");"

unwrapText :: ByteString -> ByteString
unwrapText a = "let " <> a <> " = stack.get_text().unwrap();"

pushText :: ByteString -> ByteString
pushText a = "stack.push_text(" <> a <> ");"

unwrapClosure :: ByteString -> ByteString -> ByteString
unwrapClosure a b = "let (" <> a <> "," <> b <> ") = stack.get_closure().unwrap();"

pushClosure :: ByteString -> ByteString -> ByteString
pushClosure a b = "stack.push_closure(" <> a <> "," <> b <> ");"

unwrapChar :: ByteString -> ByteString
unwrapChar a = "let " <> a <> " = stack.get_char().unwrap();"

pushChar :: ByteString -> ByteString
pushChar a = "stack.push_char(" <> a <> ");"

pushAlgebraic :: ByteString -> ByteString -> ByteString
pushAlgebraic a b = "stack.push_algebraic(" <> a <> "," <> b <> ");"

unwrapList :: ByteString -> ByteString
unwrapList a = "let " <> a <> " = stack.get_list().unwrap();"

pushList :: ByteString -> ByteString
pushList a = "stack.push_list(" <> a <> ");"

stackFn :: ByteString -> ByteString -> Bool -> ByteString
stackFn name body containsLocals =
  " fn "
    <> name
    <> "(stack: &mut Stack, closures: &Vec<Rep>) { "
    <> ( if containsLocals
           then "let mut locals: Vec<Rep> = Vec::new();" <> body
           else body
       )
    <> " }"

containsLocals :: Term a -> Bool
containsLocals t =
  any
    ( \case
        Push _ (Local _) _ -> True
        Lambda {} -> True
        Match _ _ cases e _ ->
          any (\case Case _ t _ -> containsLocals t) cases
            || ( case e of
                   DefaultElse {} -> False
                   Else t _ -> containsLocals t
               )
        _ -> False
    )
    (decompose t)

ifLetPop :: ByteString -> ByteString -> ByteString
ifLetPop binding body = matchStmt "stack.get()" [("Some(" <> binding <> ")", body), ("_", "panic!(\"Expected `Some(" <> binding <> ")`\");")]

ifLetPop2 :: (ByteString, ByteString) -> ByteString -> ByteString
ifLetPop2 (b1, b2) body = ifLetPop b1 (ifLetPop b2 body)

matchStmt :: ByteString -> [(ByteString, ByteString)] -> ByteString
matchStmt target cases = "match " <> target <> " { " <> ByteString.concat ((\(binding, block) -> binding <> " => {" <> block <> "},") <$> cases) <> " };"

letStmt :: ByteString -> ByteString -> ByteString
letStmt binding expression = "let " <> binding <> " = " <> expression <> ";"
