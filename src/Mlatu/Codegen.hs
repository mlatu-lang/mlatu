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
import Mlatu.Dictionary (Dictionary, entries)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry (Entry)
import Mlatu.Entry qualified as Entry
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Instantiated (Instantiated (..))
import Mlatu.Literal (floatValue, integerValue)
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name (ClosureIndex (..), ConstructorIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Unqualified (..))
import Mlatu.Pretty (printEntry, printInstantiated, printQualified)
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
  bs <- evalCodegen (untilM entryRs (Map.null <$> use _1)) (view entries dict, Map.empty) dict
  pure
    ( "#![feature(destructuring_assignment)] #![allow(non_snake_case, dead_code, unused_mut, unused_variables, unused_assignments, unreachable_code)]"
        <> "type StackFn = fn(Vec<Rep>, Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>); #[derive(Debug, Clone)] enum Rep { Name(StackFn), Closure(StackFn, Vec<Rep>),Algebraic(i64, Vec<Rep>), Char(char), Text(String), Int(i64), Float(f64), List(Vec<Rep>) } use Rep::*;"
        <> "#[inline] fn convertBool(b: bool, mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) { if b { mtrue(stack, closures) } else { mfalse(stack, closures) } }"
        <> "#[inline] fn convertOption(o: Option<Rep>, mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) { if let Some(x) = o { stack.push(x); msome(stack, closures) } else { mnone(stack, closures) } }"
        <> ByteString.concat bs
    )

newtype Codegen a = Codegen (StateT (Map Instantiated Entry, Map Instantiated Entry) (ReaderT Dictionary IO) a)
  deriving (Monad, Functor, Applicative, MonadState (Map Instantiated Entry, Map Instantiated Entry), MonadReader Dictionary, MonadIO)

evalCodegen :: Codegen a -> (Map Instantiated Entry, Map Instantiated Entry) -> Dictionary -> IO a
evalCodegen (Codegen c) initialState = runReaderT (evalStateT c initialState)

entryRs :: Codegen ByteString
entryRs =
  use _1
    >>= ( ( \case
              Nothing -> pure ""
              Just ((i, e), newMap) ->
                assign _1 newMap
                  >> ( case (i, e) of
                         (Instantiated name [], Entry.Word _ _ _ _ _ (Just body))
                           | name == mainName -> do
                             x <- (<> "}") . ("fn main() { let mut stack: Vec<Rep> = Vec::new(); let mut locals: Vec<Rep> = Vec::new(); let mut closures: Vec<Vec<Rep>> = Vec::new();" <>) <$> termRs body
                             modify' $ over _1 (Map.insert i e)
                             pure x
                         (_, Entry.Word _ _ _ _ _ (Just body)) -> do
                           x <- stackFn (rustifyInstantiated i) <$> termRs body
                           modify' $ over _1 (Map.insert i e)
                           pure x
                         _ -> do
                           modify' $ over _1 (Map.insert i e)
                           pure ""
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
          (DefaultElse _ _) -> word (Qualified Vocabulary.global "abort") []
        rest <- goTerms xs
        pure $ matchStmt cond (catMaybes cs ++ [("_", e)]) <> rest
      (y : ys) -> do
        a <- goTerm y
        b <- goTerms ys
        pure $ a <> b
    goTerm = \case
      Group a -> termRs a
      NewVector _ size _ _ -> pure $ letStmt "v" (vecBuilder size) <> stackPush "List(v)"
      Push _ (Character c) _ -> pure $ stackPush $ "Char('" <> show c <> "')"
      Push _ (Float f) _ -> pure $ stackPushFloat $ show val
        where
          val :: Double
          val = floatValue f
      Push _ (Integer i) _ -> pure $ stackPushInt $ show $ view integerValue i
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
      Lambda _ _ _ body _ -> do
        a <- termRs body
        pure $ "locals.push(stack.pop().unwrap());" <> a <> "locals.pop();"
      Match _ _ cases els _ -> do
        cs <- traverse caseRs cases
        e <- case els of
          (Else body _) -> termRs body
          (DefaultElse _ _) -> word (Qualified Vocabulary.global "abort") []
        pure $ matchStmt "stack.pop()" (catMaybes cs ++ [("_", e)])
      (New _ (ConstructorIndex i) size _) -> pure $ letStmt "mut v" (vecBuilder size) <> "v.reverse(); " <> stackPush ("Algebraic(" <> show i <> ", v)")
      NewClosure _ size _ -> pure $ ifLetPop "Name(n)" (letStmt "v" (vecBuilder size) <> stackPush "Closure(n, v)")
      _ -> pure ""

stackPush :: ByteString -> ByteString
stackPush x = "stack.push(" <> x <> ");"

searchLists :: Instantiated -> Codegen (Maybe Entry)
searchLists name = do
  (toBeDone, done) <- get
  pure $ case Map.lookup name toBeDone of
    Just e -> Just e
    Nothing -> Map.lookup name done

word :: Qualified -> [Type] -> Codegen ByteString
word name args = do
  isInstantiated <- isJust <$> searchLists (Instantiated name args)
  if isInstantiated
    then case Instantiated name args of
      (Instantiated (Qualified v unqualified) [])
        | v == Vocabulary.intrinsic -> intrinsic unqualified
      _ -> pure $ "(stack, closures) = " <> rustifyInstantiated (Instantiated name args) <> "(stack, closures);"
    else do
      isUninstantiated <- searchLists (Instantiated name [])
      case isUninstantiated of
        Just (Entry.Word a b c d e (Just body)) ->
          liftIO (runMlatuExceptT $ Instantiate.term TypeEnv.empty body args)
            >>= ( \case
                    Right body' -> do
                      modify' (over _1 (Map.insert (Instantiated name args) (Entry.Word a b c d e (Just body'))))
                      pure $ "(stack, closures) = " <> rustifyInstantiated (Instantiated name args) <> "(stack, closures);"
                    Left _ -> error "Could not instantiate generic type"
                )
        Just (Entry.Word _ _ _ _ _ Nothing) -> case name of
          (Qualified v unqualified)
            | v == Vocabulary.intrinsic -> intrinsic unqualified
          _ -> error "No such intrinsic"
        Just e -> pure $ "/*" <> show (printInstantiated (Instantiated name args)) <> show (printEntry e) <> "*/"
        Nothing -> pure $ "/* " <> show (printInstantiated (Instantiated name args)) <> "*/"

intrinsic :: Unqualified -> Codegen ByteString
intrinsic = \case
  "call" -> pure $ ifLetPop "Closure(n, rs)" "closures.push(rs); (stack, closures) = n(stack, closures); closures.pop();"
  "abort" -> pure $ ifLetPop "Text(a)" "panic!(\"Execution failure: {}\", a);"
  "exit" -> pure $ ifLetPop "Int(i)" "std::process::exit(i as i32);"
  "drop" -> pure $ letStmt "_" "stack.pop().unwrap();"
  "swap" -> pure $ ifLetPop2 ("a", "b") (stackPush "a" <> stackPush "b")
  "add_int64" -> binaryInt "+"
  "sub_int64" -> binaryInt "-"
  "mul_int64" -> binaryInt "*"
  "div_int64" -> binaryInt "/"
  "mod_int64" -> binaryInt "%"
  "not_int64" -> pure $ ifLetPop "Int(a)" $ stackPushInt "!a"
  "or_int64" -> binaryInt "|"
  "and_int64" -> binaryInt "&"
  "xor_int64" -> binaryInt "^"
  "gt_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ convertBool "a > b"
  "eq_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ convertBool "a == b"
  "gt_char" -> pure $ ifLetPop2 ("Char(a)", "Char(b)") $ convertBool "a > b"
  "eq_char" -> pure $ ifLetPop2 ("Char(a)", "Char(b)") $ convertBool "a == b"
  "add_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPushFloat "a + b"
  "sub_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPushFloat "a - b"
  "mul_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPushFloat "a * b"
  "div_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPushFloat "a / b"
  "mod_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPushFloat "a % b"
  "exp_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.exp()"
  "log_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.log10()"
  "sqrt_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.sqrt()"
  "sin_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.sin()"
  "cos_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.cos()"
  "tan_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.tan()"
  "asin_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.asin()"
  "acos_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.acos()"
  "atan_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.atan()"
  "atan2_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPushFloat "a.atan2(b)"
  "sinh_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.sinh()"
  "cosh_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.cosh()"
  "tanh_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.tanh()"
  "asinh_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.asinh()"
  "acosh_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.acosh()"
  "atanh_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.atanh()"
  "trunc_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.trunc()"
  "round_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.round()"
  "floor_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.floor()"
  "ceil_float64" -> pure $ ifLetPop "Float(a)" $ stackPushFloat "a.ceil()"
  "gt_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ convertBool "a > b"
  "eq_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ convertBool "(a - b).abs() < f64::EPSILON"
  "gt_string" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") $ convertBool "a > b"
  "eq_string" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") $ convertBool "a == b"
  "show_int64" -> pure $ ifLetPop "Int(a)" $ stackPushText "format!(\"{:?}\", a)"
  "show_float64" -> pure $ ifLetPop "Float(a)" $ stackPushText "format!(\"{:?}\", a)"
  "read_int64" -> pure $ ifLetPop "Text(a)" $ convertOption "a.parse().ok().map(Int)"
  "read_float64" -> pure $ ifLetPop "Text(a)" $ convertOption "a.parse().ok().map(Float)"
  "empty" -> pure $ ifLetPop "List(a)" $ convertBool "a.is_empty()"
  "cat" -> pure $ ifLetPop2 ("List(a)", "List(b)") (letStmt "mut new_vec" "b.clone()" <> "new_vec.extend(a.into_iter());" <> stackPush "List(new_vec)")
  "string_concat" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") (letStmt "mut new_string" "b" <> "new_string.push_str(&a);" <> stackPushText "new_string")
  "string_from_list" -> pure $ ifLetPop "List(a)" $ stackPushText "a.iter().filter_map(|e| if let Char(c) = e { Some(c) } else { None } ).collect::<String>()"
  "string_to_list" -> pure $ ifLetPop "Text(a)" $ stackPush "List(a.chars().map(Char).collect::<Vec<_>>())"
  "get" -> pure $ ifLetPop2 ("Int(a)", "List(b)") $ convertOption "b.get(a as usize).cloned()"
  "set" -> do
    s <- word (Qualified Vocabulary.global "some") []
    n <- word (Qualified Vocabulary.global "none") []
    pure $ ifLetPop3 ("Int(a)", "x", "List(b)") ("if a < 0 || (a as usize) >= b.len() { b[a as usize] = x;" <> stackPush "List(b)" <> s <> " } else {" <> n <> "}")
  "head" -> pure $ ifLetPop "List(a)" $ convertOption "a.first().cloned()"
  "last" -> pure $ ifLetPop "List(a)" $ convertOption "a.last().cloned()"
  "tail" -> pure $ ifLetPop "List(a)" $ convertOption "a.split_first().map(|(_, t)| List(t.to_vec()))"
  "init" -> pure $ ifLetPop "List(a)" $ convertOption "a.split_last().map(|(_, i)| List(i.to_vec()))"
  "print" -> pure $ ifLetPop "Text(a)" "println!(\"{}\", a);"
  "get_line" -> pure $ letStmt "mut buf" "String::new()" <> "std::io::stdin().read_line(&mut buf).unwrap();" <> stackPushText "buf"
  "flush_stdout" -> pure "use std::io::Write; std::io::stdout().flush().unwrap();"
  "read_file" -> pure $ ifLetPop "Text(a)" $ stackPushText "std::fs::read_to_string(a).unwrap()"
  "write_file" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") ("use std::io::Write;" <> letStmt "mut file" "std::fs::File::create(b).unwrap()" <> "file.write_all(a.as_bytes()).unwrap();")
  "append_file" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") ("use std::io::Write;" <> letStmt "mut file" "std::fs::File::open(b).unwrap()" <> "file.write_all(a.as_bytes()).unwrap();")
  x -> error ("No such intrinsic: " <> show x)
  where
    binaryInt :: ByteString -> Codegen ByteString
    binaryInt x = pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPushInt ("a " <> x <> " b")

vecBuilder :: Int -> ByteString
vecBuilder x = "vec![" <> go x "" <> "]"
  where
    go 0 acc = acc
    go 1 acc = acc <> "stack.pop().unwrap()"
    go num acc = go (num - 1) (acc <> "stack.pop().unwrap(), ")

caseRs :: Case Type -> Codegen (Maybe (ByteString, ByteString))
caseRs (Case (QualifiedName name) caseBody _) = do
  dict <- ask
  case Dictionary.lookup (Instantiated name []) dict of
    Just (Entry.Word _ _ _ _ _ (Just ctorBody)) ->
      case decompose ctorBody of
        [New _ (ConstructorIndex i) _ _] -> Just . ("Some(Algebraic(" <> show i <> ", fields))",) . ("stack.extend(fields); " <>) <$> termRs caseBody
        _ -> pure Nothing
    _ -> pure Nothing
caseRs _ = pure Nothing

rustify :: ByteString -> ByteString
rustify txt =
  let string :: String = decodeUtf8 txt
      newString :: String = "m" <> concatMap (\c -> if isAlphaNum c || c == '_' then [c] else printf "%x" (ord c)) string
   in encodeUtf8 newString

rustifyQualified :: Qualified -> ByteString
rustifyQualified = rustify . show . printQualified

rustifyInstantiated :: Instantiated -> ByteString
rustifyInstantiated = rustify . show . printInstantiated

convertBool :: ByteString -> ByteString
convertBool a = "(stack, closures) = convertBool(" <> a <> ", stack, closures);"

convertOption :: ByteString -> ByteString
convertOption a = "(stack, closures) = convertOption(" <> a <> ", stack, closures);"

stackPushFloat :: ByteString -> ByteString
stackPushFloat a = stackPush $ "Float(" <> a <> ")"

stackPushInt :: ByteString -> ByteString
stackPushInt a = stackPush $ "Int(" <> a <> ")"

stackPushText :: ByteString -> ByteString
stackPushText a = stackPush $ "Text(" <> a <> ")"

stackFn :: ByteString -> ByteString -> ByteString
stackFn name body = "#[must_use] #[inline] fn " <> name <> "(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) { let mut locals: Vec<Rep> = Vec::new();" <> body <> " (stack, closures) }"

ifLetPop :: ByteString -> ByteString -> ByteString
ifLetPop binding body = matchStmt "stack.pop()" [("Some(" <> binding <> ")", body), ("x", "panic!(\"Expected `Some(" <> binding <> ")`, but found `{:?}`\", x);")]

ifLetPop2 :: (ByteString, ByteString) -> ByteString -> ByteString
ifLetPop2 (b1, b2) body = ifLetPop b1 (ifLetPop b2 body)

ifLetPop3 :: (ByteString, ByteString, ByteString) -> ByteString -> ByteString
ifLetPop3 (b1, b2, b3) body = ifLetPop b1 (ifLetPop b2 (ifLetPop b3 body))

matchStmt :: ByteString -> [(ByteString, ByteString)] -> ByteString
matchStmt target cases = "match " <> target <> " { " <> ByteString.concat ((\(binding, block) -> binding <> " => {" <> block <> "},") <$> cases) <> " };"

letStmt :: ByteString -> ByteString -> ByteString
letStmt binding expression = "let " <> binding <> " = " <> expression <> ";"
