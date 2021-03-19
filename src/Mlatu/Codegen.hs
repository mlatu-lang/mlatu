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
import Optics
import Text.Printf (printf)
import Data.Text qualified as Text
import Data.HashMap.Strict qualified as HashMap
import Data.Char (isAlphaNum)


generate :: Dictionary -> Text 
generate dict = 
    "#![feature(destructuring_assignment)] type StackFn = fn(Vec<Rep>, Vec<Rep>, Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Rep>, Vec<Vec<Rep>>); #[derive(Debug, Clone)] enum Rep { Name(StackFn), Closure(StackFn, Vec<Rep>),Algebraic(i64, Vec<Rep>), Char(char), Text(String), Int(i64), Float(f64), List(Vec<Rep>) } use Rep::*;"
    <> Text.concat (map (entryRs dict) (Dictionary.toList dict))

entryRs :: Dictionary -> (Instantiated, Entry) -> Text
entryRs _ (Instantiated dictName@(Qualified v unqualified) _, Entry.Word _ _ _ _ _ Nothing)
    | v == Vocabulary.intrinsic = case unqualified of 
        "call" -> stackFn name $ ifLetPop "Closure(n, rs)" "closures.insert(0, rs); (stack, locals, closures) = n(stack, locals, closures); closures.remove(0);"
        "abort" -> stackFn name $ ifLetPop "Text(a)" "panic!(\"{}\", a);"
        "exit" -> stackFn name $ ifLetPop "Int(i)" "std::process::exit(i as i32);"
        "drop" -> stackFn name "let _ = stack.pop().unwrap();"
        "swap" -> stackFn name "let a = stack.pop().unwrap(); let b = stack.pop().unwrap(); stack.push(a); stack.push(b);"
        "add_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a + b)"
        "sub_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a - b)"
        "mul_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a * b)"
        "div_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a / b)"
        "mod_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a % b)"
        "not_int64" -> stackFn name $ ifLetPop "Int(a)" $ stackPush "Int(!a)"
        "or_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a | b)"
        "and_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a & b)"
        "xor_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ stackPush "Int(a ^ b)"
        "gt_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ ifElse "a > b" callTrue callFalse
        "eq_int64" -> stackFn name $ ifLetPop2 "Int(a)" "Int(b)" $ ifElse "a == b" callTrue callFalse
        "gt_char" -> stackFn name $ ifLetPop2 "Char(a)" "Char(b)" $ ifElse "a > b" callTrue callFalse
        "eq_char" -> stackFn name $ ifLetPop2 "Char(a)" "Char(b)" $ ifElse "a == b" callTrue callFalse
        "add_float64" -> stackFn name $ ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a + b)"
        "sub_float64" -> stackFn name $ ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a - b)"
        "mul_float64" -> stackFn name $ ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a * b)"
        "div_float64" -> stackFn name $ ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a / b)"
        "mod_float64" -> stackFn name $ ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a % b)"
        "exp_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.exp())"
        "log_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.log10())"
        "sqrt_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.sqrt())"
        "sin_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.sin())"
        "cos_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.cos())"
        "tan_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.tan())"
        "asin_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.asin())"
        "acos_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.acos())"
        "atan_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.atan())"
        "atan2_float64" -> stackFn name $ ifLetPop2 "Float(a)" "Float(b)" $ stackPush "Float(a.atan2(b))"
        "sinh_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.sinh())"
        "cosh_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.cosh())"
        "tanh_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.tanh())"
        "asinh_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.asinh())"
        "acosh_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.acosh())"
        "atanh_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.atanh())"
        "trunc_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.trunc())"
        "round_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.round())"
        "floor_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.floor())"
        "ceil_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Float(a.ceil())"
        "gt_float64" -> stackFn name $ ifLetPop2 "Float(a)" "Float(b)" $ ifElse "a > b" callTrue callFalse
        "eq_float64" -> stackFn name $ ifLetPop2 "Float(a)" "Float(b)" $ ifElse "a == b" callTrue callFalse
        "gt_string" -> stackFn name $ ifLetPop2 "Text(a)" "Text(b)" $ ifElse "a > b" callTrue callFalse
        "eq_string" -> stackFn name $ ifLetPop2 "Text(a)" "Text(b)" $ ifElse "a == b" callTrue callFalse
        "show_int64" -> stackFn name $ ifLetPop "Int(a)" $ stackPush "Text(format!(\"{:?}\", a))"
        "show_float64" -> stackFn name $ ifLetPop "Float(a)" $ stackPush "Text(format!(\"{:?}\", a))"
        "read_int64" -> stackFn name $ ifLetPop "Text(a)" $ ifLetElse "Ok(i)" "a.parse()" (stackPush "Int(i)" <> callSome) callNone
        "read_float64" -> stackFn name $ ifLetPop "Text(a)" $ ifLetElse "Ok(i)" "a.parse()" (stackPush "Float(i)" <> callSome) callNone
        "empty" -> stackFn name $ ifLetPop "List(a)" $ ifElse "a.is_empty()" callTrue callFalse
        "cat" -> stackFn name $ ifLetPop2 "List(a)" "List(b)" "let mut new_vec = b.clone(); new_vec.extend(a.into_iter()); stack.push(List(new_vec));"
        "string_concat" -> stackFn name $ ifLetPop2 "Text(a)" "Text(b)" "let mut new_string = b.clone(); new_string.push_str(&a); stack.push(Text(new_string));"
        "string_from_list" -> stackFn name $ ifLetPop "List(a)" $ stackPush "Text(a.iter().filter_map(|e| if let Char(c) = e { Some(c) } else { None }).collect::<String>())"
        "string_to_list" -> stackFn name $ ifLetPop "Text(a)" $ stackPush "List(a.chars().map(Char).collect::<Vec<_>>())"
        "get" -> stackFn name $ ifLetPop2 "Int(a)" "List(b)" $ ifLetElse "Some(gotten)" "b.get(a as usize)" (stackPush "gotten.clone()" <> callSome) callNone
        "set" -> stackFn name $ ifLet "(Some(Int(a)), Some(x), Some(List(b)))" "(stack.pop(), stack.pop(), stack.pop())" $ ifElse "a < 0 || (a as usize) >= b.len()" ("b[a as usize] = x;" <> stackPush "List(b)" <> callSome) callNone
        "head" -> stackFn name $ ifLetPop "List(a)" $ ifLetElse "Some(h)" "a.first()" (stackPush "h.clone()" <> callSome) callNone
        "last" -> stackFn name $ ifLetPop "List(a)" $ ifLetElse "Some(l)" "a.last()" (stackPush "l.clone()" <> callSome) callNone
        "tail" -> stackFn name $ ifLetPop "List(a)" $ ifLetElse "Some((_, t))" "a.split_first()" (stackPush "List(t.to_vec())" <> callSome) callNone
        "init" -> stackFn name $ ifLetPop "List(a)" $ ifLetElse "Some((_, i))" "a.split_last()" (stackPush "List(i.to_vec())" <> callSome) callNone
        "print" -> stackFn name $ ifLetPop "Text(a)" "println!(\"{}\", a);"
        "get_line" -> stackFn name $ "let buf = String::new(); std::io::stdin().read_line(&mut buf).unwrap();" <> stackPush "Text(buf)"
        "flush_stdout" -> stackFn name "use std::io::Write; std::io::stdout().flush().unwrap();"
        "read_file" -> stackFn name $ ifLetPop "Text(a)" $ stackPush "Text(std::fs::read_to_string(a).unwrap())"
        "write_file" -> stackFn name $ ifLetPop2 "Text(a)" "Text(b)" "use std::io::Write; let file = std::fs::File::create(b).unwrap(); file.write_all(a.as_bytes()).unwrap();"
        "append_file" -> stackFn name $ ifLetPop2 "Text(a)" "Text(b)" "use std::io::Write; let file = std::fs::File::open(b).unwrap(); file.write_all(a.as_bytes()).unwrap();"
        _ -> "" 
      where 
        name = rustifyQualified dictName
entryRs dict (Instantiated name [], Entry.Word _ _ _ _ _ (Just body))
    | name == mainName = fn "main" "" "()" $ "let mut stack = Vec::new(); let mut locals = Vec::new(); let mut closures = Vec::new();" <> termRs dict body
entryRs dict (name@(Instantiated q _), Entry.Word _ _ _ _ _ (Just body)) = stackFn (rustifyInstantiated name) (termRs dict body) 
entryRs _ _ = ""

callStatement :: Text -> Text
callStatement x = "(stack, locals, closures) = " <> x <> "(stack, locals, closures);"

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

termRs :: Dictionary -> Term a -> Text
termRs dict (Compose _ a b) =  termRs dict a <> termRs dict b
termRs dict (Generic _ _ a _) = termRs dict a
termRs dict (Group a) = termRs dict a
termRs _ (NewVector _ size _ _) = "let (new_stack, splitted) = stack.split_at((stack.len() - 1) - " <> show size <> "); stack = new_stack.to_vec(); let mut new_vec = splitted.to_vec(); new_vec.reverse();" <> stackPush "List(new_vec)"
termRs _ (Push _ v _) = valueRs v
termRs dict (Word _ _ (QualifiedName name) args _) = callStatement $ rustify $ show n where 
        mangled = Instantiated name args
        n = case Dictionary.lookup mangled dict of
                Just (Entry.Word _ _ _ _ _ (Just _)) -> printInstantiated mangled 
                _ -> printQualified name 

termRs dict (Lambda _ _ _ body _) = "locals.insert(0, stack.pop().unwrap());" <> termRs dict body <> "locals.remove(0);"
termRs dict (Match _ _  cases els _ ) = "match stack.pop() {  " <> Text.concat (map (\case
                                                                                        (Case (QualifiedName name) caseBody _) -> 
                                                                                                (case HashMap.lookup name mappings of 
                                                                                                        Just i -> "Some(Algebraic(" <> show i <> ", fields)) => { stack.extend(fields); " <> termRs dict caseBody <> " }"
                                                                                                        Nothing -> "")
                                                                                        _ -> "") cases)
                                                                                <> "Some(_) => {" <> 
                                                                                        (case els of 
                                                                                                (Else body _) -> termRs dict body
                                                                                                (DefaultElse _ _) -> callStatement (rustify "abort") ) <> "}, None => unreachable!() }" where 
                        mappings = HashMap.fromList $ mapMaybe (\case 
                                (Instantiated name [], Entry.Word _ _ _ _ _ (Just ctorBody)) -> (case decompose ctorBody of
                                                        [New _ (ConstructorIndex i) _ _] -> Just (name, i) 
                                                        _ -> Nothing)
                                _ -> Nothing ) (Dictionary.toList dict)
                                                                                                
termRs _ (New _ (ConstructorIndex i) size _) =  "let (new_stack, fields) = stack.split_at((stack.len() - 1) - " <> show size <> "); stack = new_stack.to_vec();" <> stackPush ("Algebraic(" <> show i <> ", fields.to_vec())")
termRs _ (NewClosure _ size _) = ifLetPop "Name(n)" ("let (new_stack, splitted) = stack.split_at((stack.len() - 1) - " <> show size <> "); stack = new_stack.to_vec(); let mut rs = splitted.to_vec(); rs.reverse();" <> stackPush "Closure(n, rs)")
termRs _ _ = ""

rustify :: Text -> Text 
rustify txt = toText $ "m_" <> concatMap (\c -> if isAlphaNum c || c == '_' then [c] else printf "%X" (ord c)) (toString txt)

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
valueRs (Text txt) = stackPush $ "Text(\"" <> txt <> "\".to_owned())"
valueRs (Name name) = stackPush $ "Name(" <> rustifyQualified name <> ")"
valueRs (Local (LocalIndex i)) = stackPush $ "locals[" <> show i <> "]"
valueRs (Closed (ClosureIndex i)) = stackPush $ "closures[0][" <> show i <> "]"
valueRs (Quotation {}) = "" 
valueRs (Capture {}) = ""

fn :: Text -> Text -> Text -> Text -> Text
fn name inputSig outputSig body = " fn " <> name <> "(" <> inputSig <> ") -> " <> outputSig <> "{" <> body <> "}"

stackFn :: Text -> Text -> Text
stackFn name body = fn name "mut stack: Vec<Rep>, mut locals: Vec<Rep>, mut closures: Vec<Vec<Rep>>" "(Vec<Rep>, Vec<Rep>, Vec<Vec<Rep>>)" (body <> " (stack, locals, closures) ")

ifLetPop :: Text -> Text -> Text
ifLetPop binding body = ifLet ("Some(" <> binding <> ")") "stack.pop()" body

ifLetPop2 :: Text -> Text -> Text -> Text
ifLetPop2 binding1 binding2 body = ifLet ("(Some(" <> binding1 <> "), Some(" <> binding2 <> "))") "(stack.pop(), stack.pop())" body

ifLet :: Text -> Text -> Text -> Text
ifLet binding expression body = ifLetElse binding expression body "unreachable!()"

ifLetElse :: Text -> Text -> Text -> Text -> Text
ifLetElse binding expression trueBody falseBody = ifElse ("let " <> binding <> " = " <> expression) trueBody falseBody

ifElse :: Text -> Text -> Text -> Text
ifElse cond trueBody falseBody = "if " <> cond <> " {" <> trueBody <> "} else {" <> falseBody <> "}"