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
import Mlatu.Name (GeneralName(..), Qualified(..), LocalIndex(..), ConstructorIndex(..), ClosureIndex(..))
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
                | name == mainName = (\t -> fn "main" "" "()" ("let mut stack = Vec::new(); let mut locals: Vec<Rep> = Vec::new(); let mut closures = Vec::new();" <> t)) <$> (termRs body)
        entryRs (name, Entry.Word _ _ _ _ _ (Just body)) = (stackFn (rustifyInstantiated name)) <$> (termRs body)
        entryRs _ = return ""

        callStatement :: Text -> Text
        callStatement x = "(stack, closures) = " <> x <> "(stack, closures);"

        callTrue :: Text 
        callTrue = callStatement $ rustify "true"

        callFalse :: Text 
        callFalse = callStatement $ rustify "false"

        callSome :: Text
        callSome = callStatement $ rustify "some"

        callNone :: Text 
        callNone = callStatement $ rustify "none"

        stackPush :: Text -> Text
        stackPush x = "stack.push(" <> x <> ");"

        termRs :: (Show a) =>  Term a -> IO Text
        termRs (Compose _ a b) = (termRs a) >>= (\t1 -> (termRs b) >>= (\t2 -> return (t1 <> t2)))
        termRs (Generic _ _ a _) = termRs a
        termRs (Group a) = termRs a
        termRs (NewVector _ size _ _) = return ("let v = vec![" <> vecBuilder size <> "];" <> stackPush ("List(v)"))
        termRs (Push _ v _) = return $ valueRs v
        termRs (Word _ _ (QualifiedName name) args _) = 
          let mangled = Instantiated name args
          in case Dictionary.lookup mangled dict of
            Just (Entry.Word _ _ _ _ _ (Just _)) -> return $ callStatement (rustifyInstantiated mangled)
            _ -> case Dictionary.lookup (Instantiated name []) dict of
                Just (Entry.Word _ _ _ _ _ (Just body)) -> do
                  --modifyIORef' others ((mangled, body) :)
                  --return $ callStatement (rustifyInstantiated mangled)
                  return $ callStatement (rustifyQualified name)
                Just (Entry.Word _ _ _ _ _ Nothing) -> case name of
                  Qualified v unqualified
                    | v == Vocabulary.intrinsic -> return $ case unqualified of 
                        "call" -> ifLetPop "Closure(n, rs)" "closures.push(rs); (stack, closures) = n(stack, closures); closures.pop();"
                        "abort" -> ifLetPop "Text(a)" "panic!(\"{}\", a);"
                        "exit" -> ifLetPop "Int(i)" "std::process::exit(i as i32);"
                        "drop" -> "let _ = stack.pop().unwrap();"
                        "swap" -> "let a = stack.pop().unwrap(); let b = stack.pop().unwrap();" <> stackPush "a" <> stackPush "b"
                        "add_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a + b)"
                        "sub_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a - b)"
                        "mul_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a * b)"
                        "div_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a / b)"
                        "mod_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a % b)"
                        "not_int64" -> ifLetPop "Int(a)" $ stackPush "Int(!a)"
                        "or_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a | b)"
                        "and_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a & b)"
                        "xor_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a ^ b)"
                        "gt_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ ifElse "a > b" callTrue callFalse
                        "eq_int64" -> ifLetPop2 "Int(a)" "Int(b)" $ ifElse "a == b" callTrue callFalse
                        "gt_char" -> ifLetPop2 "Char(a)" "Char(b)" $ ifElse "a > b" callTrue callFalse
                        "eq_char" -> ifLetPop2 "Char(a)" "Char(b)" $ ifElse "a == b" callTrue callFalse
                        "add_float64" -> ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a + b)"
                        "sub_float64" -> ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a - b)"
                        "mul_float64" -> ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a * b)"
                        "div_float64" -> ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a / b)"
                        "mod_float64" -> ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a % b)"
                        "exp_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.exp())"
                        "log_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.log10())"
                        "sqrt_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.sqrt())"
                        "sin_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.sin())"
                        "cos_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.cos())"
                        "tan_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.tan())"
                        "asin_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.asin())"
                        "acos_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.acos())"
                        "atan_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.atan())"
                        "atan2_float64" -> ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a.atan2(b))"
                        "sinh_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.sinh())"
                        "cosh_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.cosh())"
                        "tanh_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.tanh())"
                        "asinh_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.asinh())"
                        "acosh_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.acosh())"
                        "trunc_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.trunc())"
                        "round_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.round())"
                        "floor_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.floor())"
                        "ceil_float64" -> ifLetPop "Float(a)" $ stackPush "Float(a.ceil())"
                        "gt_float64" -> ifLetPop2 "Float(a)" "Float(b)" $ ifElse "a > b" callTrue callFalse
                        "eq_float64" -> ifLetPop2 "Float(a)" "Float(b)" $ ifElse "a == b" callTrue callFalse
                        "gt_string" -> ifLetPop2 "Text(a)" "Text(b)" $ ifElse "a > b" callTrue callFalse
                        "eq_string" -> ifLetPop2 "Text(a)" "Text(b)" $ ifElse "a == b" callTrue callFalse
                        "show_int64" -> ifLetPop "Int(a)" $ stackPush "Text(format!(\"{:?}\", a))"
                        "show_float64" -> ifLetPop "Float(a)" $ stackPush "Text(format!(\"{:?}\", a))"
                        "read_int64" -> ifLetPop "Text(a)" $ ifLetElse "Ok(i)" "a.parse()" (stackPush "Int(i)" <> callSome) callNone
                        "read_float64" -> ifLetPop "Text(a)" $ ifLetElse "Ok(i)" "a.parse()" (stackPush "Float(i)" <> callSome) callNone
                        "empty" -> ifLetPop "List(a)" $ ifElse "a.is_empty()" callTrue callFalse
                        "cat" -> ifLetPop2 "List(a)" "List(b)" "let mut new_vec = b.clone(); new_vec.extend(a.into_iter()); stack.push(List(new_vec));"
                        "string_concat" -> ifLetPop2 "Text(a)" "Text(b)" "let mut new_string = b.clone(); new_string.push_str(&a); stack.push(Text(new_string));"
                        "string_from_list" -> ifLetPop "List(a)" $ stackPush "Text(a.iter().filter_map(|e| if let Char(c) = e { Some(c) } else { None }).collect::<String>())"
                        "string_to_list" -> ifLetPop "Text(a)" $ stackPush "List(a.chars().map(Char).collect::<Vec<_>>())"
                        "get" -> ifLetPop2 "Int(a)" "List(b)" $ ifLetElse "Some(gotten)" "b.get(a as usize)" (stackPush "gotten.clone()" <> callSome) callNone
                        "set" -> ifLet "(Some(Int(a)), Some(x), Some(List(b)))" "(stack.pop(), stack.pop(), stack.pop())" $ ifElse "a < 0 || (a as usize) >= b.len()" ("b[a as usize] = x;" <> stackPush "List(b)" <> callSome) callNone
                        "head" -> ifLetPop "List(a)" $ ifLetElse "Some(h)" "a.first()" (stackPush "h.clone()" <> callSome) callNone
                        "last" -> ifLetPop "List(a)" $ ifLetElse "Some(l)" "a.last()" (stackPush "l.clone()" <> callSome) callNone
                        "tail" -> ifLetPop "List(a)" $ ifLetElse "Some((_, t))" "a.split_first()" (stackPush "List(t.to_vec())" <> callSome) callNone
                        "init" -> ifLetPop "List(a)" $ ifLetElse "Some((_, i))" "a.split_last()" (stackPush "List(i.to_vec())" <> callSome) callNone
                        "print" -> ifLetPop "Text(a)" "println!(\"{}\", a);"
                        "get_line" -> "let mut buf = String::new(); std::io::stdin().read_line(&mut buf).unwrap();" <> stackPush "Text(buf)"
                        "flush_stdout" -> "use std::io::Write; std::io::stdout().flush().unwrap();"
                        "read_file" -> ifLetPop "Text(a)" $ stackPush "Text(std::fs::read_to_string(a).unwrap())"
                        "write_file" -> ifLetPop2 "Text(a)" "Text(b)" "use std::io::Write; let mut file = std::fs::File::create(b).unwrap(); file.write_all(a.as_bytes()).unwrap();"
                        "append_file" -> ifLetPop2 "Text(a)" "Text(b)" "use std::io::Write; let mut file = std::fs::File::open(b).unwrap(); file.write_all(a.as_bytes()).unwrap();"
                        _ -> ""
                  _ -> return ""
                _ -> return ""
        termRs (Lambda _ _ _ body _) = (termRs body) >>= (\t -> return $ "locals.push(stack.pop().unwrap());" <> t <> "locals.pop();")
        termRs (Match _ _  cases els _ ) = do 
                cs <- mapM caseRs cases
                e <- case els of
                        (Else body _) -> termRs body
                        (DefaultElse _ _) -> return $ callStatement (rustify "abort")
                return $ ifLetPop "a" (matchStmt "a" (catMaybes cs ++ [("_", e)]))                                                 
        termRs (New _ (ConstructorIndex i) size _) = return ("let mut v = vec![" <> vecBuilder size  <> "]; v.reverse(); " <> stackPush ("Algebraic(" <> show i <> ", v)"))
        termRs (NewClosure _ size _) = return $ ifLetPop "Name(n)" $ ("let v = vec![" <> vecBuilder size  <> "];" <> stackPush "Closure(n, v)")
        termRs _ = return ""

        vecBuilder :: Int -> Text 
        vecBuilder x = go x "" where 
                go 0 acc = acc
                go 1 acc = acc <> "stack.pop().unwrap()"
                go num acc = go (num - 1) (acc <> "stack.pop().unwrap(), ")

        caseRs :: (Show a) => Case a -> IO (Maybe (Text, Text))
        caseRs (Case (QualifiedName name) caseBody _) = 
          case Dictionary.lookup (Instantiated name []) dict of 
           Just (Entry.Word _ _ _ _ _ (Just ctorBody)) -> 
            case decompose ctorBody of 
              [New _ (ConstructorIndex i) _ _] -> (\t -> Just ("Algebraic(" <> show i <> ", fields)", "stack.extend(fields); " <> t)) <$> (termRs caseBody)
              _ -> return Nothing
           _ -> return Nothing
        caseRs _ = return Nothing


        rustify :: Text -> Text 
        rustify txt = toText $ "m" <> concatMap (\c -> if isAlphaNum c || c == '_' then [c] else printf "%x" (ord c)) (toString txt)

        rustifyQualified :: Qualified -> Text 
        rustifyQualified = rustify . show . printQualified

        rustifyInstantiated :: Instantiated -> Text 
        rustifyInstantiated = rustify . show . printInstantiated

        valueRs :: Value a -> Text
        valueRs (Character c) = stackPush $ "Char('" <> show c <> "')"
        valueRs (Float f) = stackPush $ "Float(" <> show val <> ")"
                where 
                        val :: Double 
                        val = floatValue f
        valueRs (Integer i) =  stackPush $ "Int(" <> show (view integerValue i) <> ")"
        valueRs (Text txt) = stackPush $ "Text(\"" <> (toText $ concatMap
                ( \case
                    '\n' -> "\\n"
                    c -> [c]
                )
                (toString txt)) <> "\".to_owned())"
        valueRs (Name name) = stackPush $ "Name(" <> rustifyQualified name <> ")"
        valueRs (Local (LocalIndex i)) = stackPush $ case i of 
                        0 -> "locals.last().unwrap().clone()"
                        _ -> "locals[locals.len() - " <> show (1 + i) <> "].clone()"
        valueRs (Closed (ClosureIndex i)) = stackPush $ "closures.last().unwrap().clone()[" <> show i <> "].clone()"
        valueRs (Quotation {}) = "" 
        valueRs (Capture {}) = ""

        fn :: Text -> Text -> Text -> Text -> Text
        fn name inputSig outputSig body = " fn " <> name <> "(" <> inputSig <> ") -> " <> outputSig <> "{" <> body <> "}"

        stackFn :: Text -> Text -> Text
        stackFn name body = fn name "mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>" "(Vec<Rep>, Vec<Vec<Rep>>)" ("let mut locals: Vec<Rep> = Vec::new();" <> body <> " (stack, closures) ")

        ifLetPop :: Text -> Text -> Text
        ifLetPop binding body = ifLet ("Some(" <> binding <> ")") "stack.pop()" body

        ifLetPop2 :: Text -> Text -> Text -> Text
        ifLetPop2 binding1 binding2 body = ifLet ("(Some(" <> binding1 <> "), Some(" <> binding2 <> "))") "(stack.pop(), stack.pop())" body

        ifLet :: Text -> Text -> Text -> Text
        ifLet binding expression body = ifLetElse binding expression body "unreachable!()"

        ifLetElse :: Text -> Text -> Text -> Text -> Text
        ifLetElse binding expression trueBody falseBody = ifElse ("let " <> binding <> " = " <> expression) trueBody falseBody

        ifElse :: Text -> Text -> Text -> Text
        ifElse cond trueBody falseBody = "if " <> cond <> " {" <> trueBody <> "} else {" <> falseBody <> "};"

        matchStmt :: Text -> [(Text, Text)] -> Text
        matchStmt target cases = "match " <> target <> " { " <> Text.concat (map (\(binding, block) -> binding <> " => {" <> block <> "},") cases) <> " };"

    entries <- forM (Dictionary.toList dict) entryRs
    otherFns <- readIORef others 
    otherTxts <- mapM (\(Instantiated name args, body) -> do 
            mBody' <- runMlatuExceptT $ Instantiate.term TypeEnv.empty body args
            case mBody' of
                Right body' -> (stackFn (rustifyInstantiated (Instantiated name args))) <$> (termRs body')
                Left _ -> return ""
        ) (ordNub otherFns)
    return $ "#![feature(destructuring_assignment)] type StackFn = fn(Vec<Rep>, Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>); #[derive(Debug, Clone)] enum Rep { Name(StackFn), Closure(StackFn, Vec<Rep>),Algebraic(i64, Vec<Rep>), Char(char), Text(String), Int(i64), Float(f64), List(Vec<Rep>) } use Rep::*;" 
        <> Text.concat entries <> Text.concat otherTxts