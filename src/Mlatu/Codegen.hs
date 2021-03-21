-- |
-- Module      : Mlatu.Interpret
-- Description : Simple interpreter
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Codegen
  ( generate
  )
where

import Relude hiding (Compose)
import Mlatu.Term (Value(..), Term(..), Else(..), Case(..), decompose)
import Mlatu.Dictionary qualified as Dictionary 
import Mlatu.Dictionary (Dictionary)
import Mlatu.Vocabulary qualified as Vocabulary
import Mlatu.Entry (Entry)
import Mlatu.Name (GeneralName(..), Qualified(..), LocalIndex(..), Unqualified(..), ConstructorIndex(..), ClosureIndex(..))
import Mlatu.Entry qualified as Entry 
import Mlatu.Instantiated (Instantiated(..))
import Mlatu.Literal (floatValue, integerValue)
import Mlatu.Pretty (printQualified, printInstantiated)
import Mlatu.Definition (mainName)
import Mlatu.Monad (runMlatuExceptT)
import Optics
import Text.Printf (printf)
import Data.Text qualified as Text
import Data.Char (isAlphaNum)
import Mlatu.TypeEnv qualified as TypeEnv 
import Mlatu.Instantiate qualified as Instantiate


generate :: Dictionary -> IO Text 
generate dict = do
    others <- newIORef []
    let entryRs ::  (Instantiated, Entry) -> IO Text
        entryRs (Instantiated name [], Entry.Word _ _ _ _ _ (Just body))
                | name == mainName = (\t -> fn "main" "" "()" ("let mut stack = Vec::new(); let mut locals: Vec<Rep> = Vec::new(); let mut closures = Vec::new();" <> t)) <$> termRs body
        entryRs (name, Entry.Word _ _ _ _ _ (Just body)) = stackFn (rustifyInstantiated name) <$> termRs body
        entryRs _ = pure ""

        stackPush :: Text -> Text
        stackPush x = "stack.push(" <> x <> ");"

        word name args =
          let mangled = Instantiated name args
          in case Dictionary.lookup mangled dict of
            Just (Entry.Word _ _ _ _ _ (Just _)) -> pure $ "(stack, closures) = " <> rustifyInstantiated mangled <> "(stack, closures);"
            _ -> case Dictionary.lookup (Instantiated name []) dict of
                Just (Entry.Word _ _ _ _ _ (Just body)) -> do
                  modifyIORef' others ((mangled, body) :)
                  traceM ("Adding " <> toString (rustifyInstantiated mangled))
                  pure $ "(stack, closures) = " <> rustifyInstantiated mangled <> "(stack, closures);"
                Just (Entry.Word _ _ _ _ _ Nothing) -> case name of
                  Qualified v unqualified
                    | v == Vocabulary.intrinsic -> intrinsic unqualified
                  _ -> pure ""
                _ -> pure ""


        intrinsic :: Unqualified -> IO Text 
        intrinsic = \case
                "call" -> pure $ ifLetPop "Closure(n, rs)" "closures.push(rs); (stack, closures) = n(stack, closures); closures.pop();"
                "abort" -> pure $ ifLetPop "Text(a)" "panic!(\"Execution failure: {}\", a);"
                "exit" -> pure $ ifLetPop "Int(i)" "std::process::exit(i as i32);"
                "drop" -> pure $ letStmt "_" "stack.pop().unwrap();"
                "swap" -> pure $ letStmt "a" "stack.pop().unwrap()" <> letStmt "b" "stack.pop().unwrap()" <> stackPush "a" <> stackPush "b"
                "add_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPush "Int(a + b)"
                "sub_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPush "Int(a - b)"
                "mul_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPush "Int(a * b)"
                "div_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPush "Int(a / b)"
                "mod_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPush "Int(a % b)"
                "not_int64" -> pure $ ifLetPop "Int(a)" $ stackPush "Int(!a)"
                "or_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPush "Int(a | b)"
                "and_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPush "Int(a & b)"
                "xor_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") $ stackPush "Int(a ^ b)"
                "gt_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") "(stack, closures) = convertBool(a > b, stack, closures);" 
                "eq_int64" -> pure $ ifLetPop2 ("Int(a)", "Int(b)") "(stack, closures) = convertBool(a == b, stack, closures);" 
                "gt_char" -> pure $ ifLetPop2 ("Char(a)", "Char(b)") "(stack, closures) = convertBool(a > b, stack, closures);" 
                "eq_char" -> pure $ ifLetPop2 ("Char(a)", "Char(b)") "(stack, closures) = convertBool(a == b, stack, closures);" 
                "add_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPush "Float(a + b)"
                "sub_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPush "Float(a - b)"
                "mul_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPush "Float(a * b)"
                "div_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPush "Float(a / b)"
                "mod_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPush "Float(a % b)"
                "exp_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.exp())"
                "log_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.log10())"
                "sqrt_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.sqrt())"
                "sin_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.sin())"
                "cos_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.cos())"
                "tan_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.tan())"
                "asin_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.asin())"
                "acos_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.acos())"
                "atan_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.atan())"
                "atan2_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") $ stackPush "Float(a.atan2(b))"
                "sinh_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.sinh())"
                "cosh_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.cosh())"
                "tanh_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.tanh())"
                "asinh_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.asinh())"
                "acosh_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.acosh())"
                "trunc_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.trunc())"
                "round_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.round())"
                "floor_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.floor())"
                "ceil_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Float(a.ceil())"
                "gt_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") "(stack, closures) = convertBool(a > b, stack, closures);" 
                "eq_float64" -> pure $ ifLetPop2 ("Float(a)", "Float(b)") "(stack, closures) = convertBool(a == b, stack, closures);" 
                "gt_string" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") "(stack, closures) = convertBool(a > b, stack, closures);" 
                "eq_string" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") "(stack, closures) = convertBool(a == b, stack, closures);" 
                "show_int64" -> pure $ ifLetPop "Int(a)" $ stackPush "Text(format!(\"{:?}\", a))"
                "show_float64" -> pure $ ifLetPop "Float(a)" $ stackPush "Text(format!(\"{:?}\", a))"
                "read_int64" -> pure $ ifLetPop "Text(a)" "(stack, closures) = convertOption(a.parse().ok().map(Int), stack, closures);" 
                "read_float64" -> pure $ ifLetPop "Text(a)" "(stack, closures) = convertOption(a.parse().ok().map(Float), stack, closures);" 
                "empty" -> pure $ ifLetPop "List(a)" "(stack, closures) = convertBool(a.is_empty(), stack, closures);" 
                "cat" -> pure $ ifLetPop2 ("List(a)", "List(b)") (letStmt "mut new_vec" "b.clone()" <> "new_vec.extend(a.into_iter());" <> stackPush "List(new_vec)")
                "string_concat" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") (letStmt "mut new_string" "b" <> "new_string.push_str(&a);" <> stackPush "Text(new_string)")
                "string_from_list" -> pure $ ifLetPop "List(a)" $ stackPush ("Text(a.iter().filter_map(|e| " <> ifLetElse "Char(c)" "e" "Some(c)" "None" <> ").collect::<String>())")
                "string_to_list" -> pure $ ifLetPop "Text(a)" $ stackPush "List(a.chars().map(Char).collect::<Vec<_>>())"
                "get" -> pure $ ifLetPop2 ("Int(a)", "List(b)") "(stack, closures) = convertOption(b.get(a as usize).cloned(), stack, closures);" 
                "set" -> do 
                        s <- word (Qualified Vocabulary.global "some") []
                        n <- word (Qualified Vocabulary.global "none") []
                        pure $ ifLetPop3 ("Int(a)", "x", "List(b)") $ ifElse "a < 0 || (a as usize) >= b.len()" ("b[a as usize] = x;" <> stackPush "List(b)" <> s) n
                "head" -> pure $ ifLetPop "List(a)" "(stack, closures) = convertOption(a.first().cloned(), stack, closures);"
                "last" -> pure $ ifLetPop "List(a)" "(stack, closures) = convertOption(a.last().cloned(), stack, closures);"
                "tail" -> pure $ ifLetPop "List(a)" "(stack, closures) = convertOption(a.split_first().map(|(_, t)| List(t.to_vec())), stack, closures);"
                "init" -> pure $ ifLetPop "List(a)" "(stack, closures) = convertOption(a.split_last().map(|(_, i)| List(i.to_vec())), stack, closures);"
                "print" -> pure $ ifLetPop "Text(a)" "println!(\"{}\", a);"
                "get_line" -> pure $ letStmt "mut buf" "String::new()" <> "std::io::stdin().read_line(&mut buf).unwrap();" <> stackPush "Text(buf)"
                "flush_stdout" -> pure "use std::io::Write; std::io::stdout().flush().unwrap();"
                "read_file" -> pure $ ifLetPop "Text(a)" $ stackPush "Text(std::fs::read_to_string(a).unwrap())"
                "write_file" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") ("use std::io::Write;" <> letStmt "mut file" "std::fs::File::create(b).unwrap()" <> "file.write_all(a.as_bytes()).unwrap();")
                "append_file" -> pure $ ifLetPop2 ("Text(a)", "Text(b)") ("use std::io::Write;" <> letStmt "mut file" "std::fs::File::open(b).unwrap()" <> "file.write_all(a.as_bytes()).unwrap();")
                _ -> pure ""

        termRs :: (Show a) =>  Term a -> IO Text
        termRs (Compose _ a b) = termRs a >>= (\t1 -> termRs b >>= (\t2 -> pure (t1 <> t2)))
        termRs (Generic _ _ a _) = termRs a
        termRs (Group a) = termRs a
        termRs (NewVector _ size _ _) = pure $ letStmt "v" (vecBuilder size) <> stackPush "List(v)"
        termRs (Push _ v _) = pure $ case v of
                Character c -> stackPush $ "Char('" <> show c <> "')"
                Float f -> stackPush $ "Float(" <> show val <> ")"
                        where val :: Double 
                              val = floatValue f
                Integer i -> stackPush $ "Int(" <> show (view integerValue i) <> ")"
                Text txt -> stackPush $ "Text(\"" <> toText (concatMap
                        ( \case
                        '\n' -> "\\n"
                        c -> [c]
                        )
                        (toString txt)) <> "\".to_owned())"
                Name name -> stackPush $ "Name(" <> rustifyQualified name <> ")"
                Local (LocalIndex i) -> stackPush $ case i of 
                        0 -> "locals.last().unwrap().clone()"
                        _ -> "locals[locals.len() - " <> show (1 + i) <> "].clone()"
                Closed (ClosureIndex i) -> stackPush $ "closures.last().unwrap().clone()[" <> show i <> "].clone()"
                Quotation {} -> "" 
                Capture {} -> ""
        termRs (Word _ _ (QualifiedName name) args _) = word name args
        termRs (Lambda _ _ _ body _) = termRs body >>= (\t -> pure $ "locals.push(stack.pop().unwrap());" <> t <> "locals.pop();")
        termRs (Match _ _  cases els _ ) = do 
                cs <- traverse caseRs cases
                e <- case els of
                        (Else body _) -> termRs body
                        (DefaultElse _ _) -> word (Qualified Vocabulary.global "abort") []
                pure $ ifLetPop "a" (matchStmt "a" (catMaybes cs ++ [("_", e)]))                                                 
        termRs (New _ (ConstructorIndex i) size _) = pure $ letStmt "mut v" (vecBuilder size) <> "v.reverse(); " <> stackPush ("Algebraic(" <> show i <> ", v)")
        termRs (NewClosure _ size _) = pure $ ifLetPop "Name(n)" (letStmt "v" (vecBuilder size) <> stackPush "Closure(n, v)")
        termRs _ = pure ""

        vecBuilder :: Int -> Text 
        vecBuilder x = "vec![" <> go x "" <> "]" where 
                go 0 acc = acc
                go 1 acc = acc <> "stack.pop().unwrap()"
                go num acc = go (num - 1) (acc <> "stack.pop().unwrap(), ")

        caseRs :: (Show a) => Case a -> IO (Maybe (Text, Text))
        caseRs (Case (QualifiedName name) caseBody _) = 
          case Dictionary.lookup (Instantiated name []) dict of 
           Just (Entry.Word _ _ _ _ _ (Just ctorBody)) -> 
            case decompose ctorBody of 
              [New _ (ConstructorIndex i) _ _] -> (\t -> Just ("Algebraic(" <> show i <> ", fields)", "stack.extend(fields); " <> t)) <$> termRs caseBody
              _ -> pure Nothing
           _ -> pure Nothing
        caseRs _ = pure Nothing

        rustify :: Text -> Text 
        rustify txt = toText $ "m" <> concatMap (\c -> if isAlphaNum c || c == '_' then [c] else printf "%x" (ord c)) (toString txt)

        rustifyQualified :: Qualified -> Text 
        rustifyQualified = rustify . show . printQualified

        rustifyInstantiated :: Instantiated -> Text 
        rustifyInstantiated = rustify . show . printInstantiated

        fn :: Text -> Text -> Text -> Text -> Text
        fn name inputSig outputSig body = " fn " <> name <> "(" <> inputSig <> ") -> " <> outputSig <> "{" <> body <> "}"

        stackFn :: Text -> Text -> Text
        stackFn name body = "#[must_use] #[inline]" <> fn name "mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>" "(Vec<Rep>, Vec<Vec<Rep>>)" ("let mut locals: Vec<Rep> = Vec::new();" <> body <> " (stack, closures) ")

        ifLetPop :: Text -> Text -> Text
        ifLetPop binding body = letStmt "x" "stack.pop()" <> ifLetElse ("Some(" <> binding <> ")") "x" body ("panic!(\"Expected `Some(" <> binding <> ")`, but found `{:?}`\", x);")

        ifLetPop2 :: (Text, Text) -> Text -> Text
        ifLetPop2 (b1, b2) body = ifLetPop b1 (ifLetPop b2 body)

        ifLetPop3 :: (Text, Text, Text) -> Text -> Text
        ifLetPop3 (b1, b2, b3) body = ifLetPop b1 (ifLetPop b2 (ifLetPop b3 body))

        ifLetElse :: Text -> Text -> Text -> Text -> Text
        ifLetElse binding expression trueBody falseBody = ifElse ("let " <> binding <> " = " <> expression) trueBody falseBody

        ifElse :: Text -> Text -> Text -> Text
        ifElse cond trueBody falseBody = "if " <> cond <> " {" <> trueBody <> "} else {" <> falseBody <> "}"

        matchStmt :: Text -> [(Text, Text)] -> Text
        matchStmt target cases = "match " <> target <> " { " <> Text.concat (fmap (\(binding, block) -> binding <> " => {" <> block <> "},") cases) <> " };"

        letStmt :: Text -> Text -> Text 
        letStmt binding expression = "let " <> binding <> " = " <> expression <> ";"

    entries <- forM (Dictionary.toList dict) entryRs
    otherFns <- readIORef others 
    otherTxts <- traverse (\(Instantiated name args, body) -> do 
            mBody' <- runMlatuExceptT $ Instantiate.term TypeEnv.empty body args
            case mBody' of
                Right body' -> stackFn (rustifyInstantiated (Instantiated name args)) <$> termRs body'
                Left _ -> error "there was a problem"
        ) (ordNub otherFns)
    let repDefinition = "type StackFn = fn(Vec<Rep>, Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>); #[derive(Debug, Clone)] enum Rep { Name(StackFn), Closure(StackFn, Vec<Rep>),Algebraic(i64, Vec<Rep>), Char(char), Text(String), Int(i64), Float(f64), List(Vec<Rep>) } use Rep::*;"
    convertBoolDefinition <- do 
            t <- word (Qualified Vocabulary.global "true") []
            f <- word (Qualified Vocabulary.global "false") []
            pure $ "fn convertBool(b: bool, stack: Vec<Rep>, closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) { if b { " <> t <> "(stack, closures) } else { " <> f <> "(stack, closures) } }" 
    convertOptionDefinition <- do 
            s <- word (Qualified Vocabulary.global "some") []
            n <- word (Qualified Vocabulary.global "none") []
            pure $ "fn convertOption(o: Option<Rep>, stack: Vec<Rep>, closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) { if let Some(x) = o { stack.push(x); " <> s <> "(stack, closures) } else { " <> n <> "(stack, closures) } }"
    pure $ "#![feature(destructuring_assignment)] #![allow(warnings)] " 
        <> repDefinition 
        <> convertBoolDefinition
        <> convertOptionDefinition
        <> Text.concat entries 
        <> Text.concat otherTxts