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
import Mlatu.Term (Case (..), Else (..), Term (..), Value (..), decompose)
import Mlatu.Type (Type)
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Vocabulary qualified as Vocabulary
import Optics
import Relude hiding (Compose, Type)
import Text.Printf (printf)

instance IsString a => IsString (Codegen a) where
  fromString = pure . fromString

generate :: Dictionary -> IO ByteString
generate dict = do
  bs <- evalCodegen (untilM entryRs (Map.null <$> getToDo)) (Map.mapKeys rustifyInstantiated (view wordEntries dict), Map.empty) dict
  pure
    ( "#![feature(destructuring_assignment)] #![allow(non_snake_case, dead_code, unused_mut, unused_variables, unused_assignments, unreachable_code)]"
        <> "fn main() { mmain(&mut Vec::new(), &mut Vec::new()); }"
        <> "type StackFn = fn(&mut Vec<Rep>, &mut Vec<Vec<Rep>>); #[derive(Clone)] enum Rep { Name(StackFn), Closure(StackFn, Vec<Rep>), Algebraic(i64, Vec<Rep>), Nat(usize), Char(char), Text(String) } use Rep::*;"
        <> "fn toList(mut n: Vec<Rep>) -> Rep { if n.is_empty() { Algebraic(0, Vec::new()) } else { let x = n.remove(0); Algebraic(1, vec![x, toList(n)]) } }"
        <> "fn fromList(n: Rep) -> Option<Vec<Rep>> { if let Algebraic(0, v) = n { if v.is_empty() { Some(vec![]) } else { None } } else if let Algebraic(1, v) = n { if let Some(ns) = fromList(v[1].clone()) { let mut ns = ns; ns.insert(0, ns[0].clone()); Some(ns) } else { None }} else { None } } "
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
      (Group a : Match _ _ cases els _ : xs) -> do
        cond <- termRs a
        cs <- traverse caseRs cases
        e <- case els of
          (Else body _) -> termRs body
          (DefaultElse _ _) -> word (Qualified Vocabulary.global "abort-now") []
        rest <- goTerms xs
        pure $ matchStmt cond (catMaybes cs ++ [("_", e)]) <> rest
      (y : ys) -> do
        a <- goTerm y
        b <- goTerms ys
        pure $ a <> b
    goTerm = \case
      Group a -> termRs a
      Push _ (Character c) _ -> pure $ stackPush $ "Char('" <> show c <> "')"
      Push _ (Text txt) _ ->
        pure $
          stackPush $
            "Text(\""
              <> encodeUtf8
                ( concatMap
                    ( \case
                        '\n' -> "\\n"
                        c -> [c]
                    )
                    (toString txt)
                )
              <> "\".to_owned())"
      Push _ (Name name) _ -> pure $ stackPush $ "Name(" <> rustifyQualified name <> ")"
      Push _ (Local (LocalIndex 0)) _ -> pure $ stackPush "locals.last().unwrap().clone()"
      Push _ (Local (LocalIndex i)) _ -> pure $ stackPush $ "locals[locals.len() - " <> show (1 + i) <> "].clone()"
      Push _ (Closed (ClosureIndex i)) _ -> pure $ stackPush $ "closures.last().unwrap().clone()[" <> show i <> "].clone()"
      Word _ (QualifiedName name) args _ -> word name args
      Lambda _ _ _ body _ -> (\a -> "locals.push(stack.pop().unwrap());" <> a <> "locals.pop();") <$> termRs body
      Match _ _ cases els _ -> do
        cs <- traverse caseRs cases
        e <- case els of
          (Else body _) -> termRs body
          (DefaultElse _ _) -> word (Qualified Vocabulary.global "abort-now") []
        pure $ matchStmt "stack.pop()" (catMaybes cs ++ [("_", e)])
      (New _ (ConstructorIndex 0) 0 True _) -> pure $ stackPush "Nat(0)"
      (New _ (ConstructorIndex 1) 1 True _) -> pure $ ifLetPop "Nat(a)" $ stackPush "Nat(a + 1)"
      (New _ (ConstructorIndex i) size False _) -> pure $ case size of
        0 -> stackPushAlgebraic (show i) "Vec::new()"
        1 -> letStmt "v" "vec![stack.pop().unwrap()]" <> stackPushAlgebraic (show i) "v"
        _ -> letStmt "mut v" (vecBuilder size) <> "v.reverse(); " <> stackPushAlgebraic (show i) "v"
      NewClosure _ size _ -> pure $
        ifLetPop "Name(n)" $ case size of
          0 -> stackPush "Closure(n, Vec::new())"
          _ -> letStmt "v" (vecBuilder size) <> stackPush "Closure(n, v)"
      _ -> pure ""

stackPush :: ByteString -> ByteString
stackPush x = "stack.push(" <> x <> ");"

constructor :: Int -> Int -> Bool -> ByteString
constructor 0 0 True = stackPush "Nat(0)"
constructor 1 1 True = ifLetPop "Nat(a)" $ stackPush "Nat(a+1)"
constructor i 0 False = stackPushAlgebraic (show i) "Vec::new()"
constructor i 1 False = letStmt "v" "vec![stack.pop().unwrap()]" <> stackPushAlgebraic (show i) "v"
constructor i size False = letStmt "mut v" (vecBuilder size) <> "v.reverse(); " <> stackPushAlgebraic (show i) "v"

word :: Qualified -> [Type] -> Codegen ByteString
word name args = do
  let mangled = rustifyInstantiated $ Instantiated name args
  isInstantiated <- uncurry ((. Map.lookup mangled) . (<|>) . Map.lookup mangled) <$> get
  case isInstantiated of
    Just (Entry.WordEntry _ _ _ _ _ b) ->
      case Instantiated name args of
        (Instantiated (Qualified v unqualified) [])
          | v == Vocabulary.intrinsic -> intrinsic unqualified
        _ -> case b of
          Just body -> case decompose body of
            [New _ (ConstructorIndex i) size b _] -> pure $ constructor i size b
            _ -> pure $ mangled <> "(stack, closures);"
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
        Just (Entry.WordEntry _ _ _ _ _ Nothing) -> case name of
          (Qualified v unqualified)
            | v == Vocabulary.intrinsic -> intrinsic unqualified
          _ -> error "No such intrinsic"
        _ -> pure ""

intrinsic :: Unqualified -> Codegen ByteString
intrinsic = \case
  "call" -> pure $ ifLetPop "Closure(n, rs)" "closures.push(rs); n(stack, closures); closures.pop();"
  "abort" -> pure $ ifLetPop "Text(a)" "panic!(\"Execution failure: {}\", a);"
  "exit" -> pure $ ifLetPop "Nat(i)" "std::process::exit(i as i32);"
  "drop" -> pure $ letStmt "_" "stack.pop().unwrap();"
  "swap" -> pure $ ifLetPop2 ("a", "b") (stackPush "a" <> stackPush "b")
  "cmp-char" -> ifLetPop2 ("Char(a)", "Char(b)") <$> cmp "a" "b"
  "cmp-string" -> ifLetPop2 ("Text(a)", "Text(b)") <$> cmp "a" "b"
  "show-nat" -> pure $ ifLetPop "Nat(i)" "stack.push(Text(format!(\"{}\", i)));"
  "read-nat" -> do
    s <- word (Qualified Vocabulary.global "some") []
    n <- word (Qualified Vocabulary.global "none") []
    pure $
      ifLetPop "Text(a)" $
        "if let Some(a) = a.parse().ok().map(Nat) {"
          <> stackPush "a"
          <> s
          <> "} else {"
          <> n
          <> "}"
  "string-concat" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") (letStmt "mut new_string" "b" <> "new_string.push_str(&a);" <> stackPushText "new_string")
  "string-from-list" -> pure $ "if let Some(a) = fromList(stack.pop().unwrap()) {" <> stackPushText "a.iter().filter_map(|e| if let Char(c) = e { Some(c) } else { None } ).collect::<String>()" <> "}"
  "string-to-list" -> pure $ ifLetPop "Text(a)" $ stackPush "toList(a.chars().map(Char).collect::<Vec<_>>())"
  "print" -> pure $ ifLetPop "Text(a)" "print!(\"{}\", a);"
  "get-line" -> pure $ letStmt "mut buf" "String::new()" <> "std::io::stdin().read_line(&mut buf).unwrap();" <> stackPushText "buf"
  "flush-stdout" -> pure "use std::io::Write; std::io::stdout().flush().unwrap();"
  "read-file" -> pure $ ifLetPop "Text(a)" $ stackPushText "std::fs::read_to_string(a).unwrap()"
  "write-file" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") ("use std::io::Write;" <> letStmt "mut file" "std::fs::File::create(b).unwrap()" <> "file.write_all(a.as_bytes()).unwrap();")
  "append-file" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") ("use std::io::Write;" <> letStmt "mut file" "std::fs::File::open(b).unwrap()" <> "file.write_all(a.as_bytes()).unwrap();")
  x -> error ("No such intrinsic: " <> show x)
  where
    cmp a b = do
      m <- word (Qualified Vocabulary.global "more") []
      l <- word (Qualified Vocabulary.global "less") []
      e <- word (Qualified Vocabulary.global "equal") []
      pure $
        matchStmt
          (a <> ".cmp(&" <> b <> ")")
          [ ("std::cmp::Ordering::Greater", m),
            ("std::cmp::Ordering::Less", l),
            ("std::cmp::Ordering::Equal", e)
          ]

vecBuilder :: Int -> ByteString
vecBuilder 0 = "Vec::new()"
vecBuilder x = "vec![" <> go x <> "]"
  where
    go 1 = "stack.pop().unwrap()"
    go num = "stack.pop().unwrap(), " <> go (num - 1)

caseRs :: Case Type -> Codegen (Maybe (ByteString, ByteString))
caseRs (Case (QualifiedName name) caseBody _) = do
  dict <- ask
  case Dictionary.lookupWord (Instantiated name []) dict of
    Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ (ConstructorIndex i) 0 False _] -> Just . ("Some(Algebraic(" <> show i <> ", _))",) <$> termRs caseBody
        [New _ (ConstructorIndex i) _ False _] -> Just . ("Some(Algebraic(" <> show i <> ", fields))",) . ("stack.extend(fields); " <>) <$> termRs caseBody
        [New _ (ConstructorIndex 0) 0 True _] -> Just . ("Some(Nat(0))",) <$> termRs caseBody
        [New _ (ConstructorIndex 1) 1 True _] -> Just . ("Some(Nat(a)) if a > 0 ",) . ("stack.push(Nat(a-1));" <>) <$> termRs caseBody
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

stackPushText :: ByteString -> ByteString
stackPushText a = stackPush $ "Text(" <> a <> ")"

stackPushAlgebraic :: ByteString -> ByteString -> ByteString
stackPushAlgebraic a b = stackPush $ "Algebraic(" <> a <> "," <> b <> ")"

stackFn :: ByteString -> ByteString -> Bool -> ByteString
stackFn name body containsLocals =
  "fn "
    <> name
    <> "(stack: &mut Vec<Rep>, closures: &mut Vec<Vec<Rep>>) { "
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
ifLetPop binding body = matchStmt "stack.pop()" [("Some(" <> binding <> ")", body), ("_", "panic!(\"Expected `Some(" <> binding <> ")`\");")]

ifLetPop2 :: (ByteString, ByteString) -> ByteString -> ByteString
ifLetPop2 (b1, b2) body = ifLetPop b1 (ifLetPop b2 body)

matchStmt :: ByteString -> [(ByteString, ByteString)] -> ByteString
matchStmt target cases = "match " <> target <> " { " <> ByteString.concat ((\(binding, block) -> binding <> " => {" <> block <> "},") <$> cases) <> " };"

letStmt :: ByteString -> ByteString -> ByteString
letStmt binding expression = "let " <> binding <> " = " <> expression <> ";"
