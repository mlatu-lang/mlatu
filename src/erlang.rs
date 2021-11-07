use std::collections::HashMap;
use std::process::Stdio;

use combine::parser::{char, repeat};
use combine::{between, Parser};
use tokio::fs;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::Command;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};

use crate::ast::{Rule, Term};

pub const BEFORE:&str = "-module(mlatu). -export([rewrite/1]).
myappend({cons, X, Xs}, Rest) -> {cons, X, myappend(Xs, Rest)};
myappend(nil, Rest) -> Rest.
";

pub const AFTER:&str = "
match({cons,{word,'u'}, {cons,{quotation, A}, Rest}},Reset) ->  
   rewrite(Reset(myappend(A,Rest)));
match({cons, {word, 'q'}, {cons, A, Rest}},Reset) -> 
   rewrite(Reset({cons,{quotation, [A]}, Rest}));
match({cons, {word, 'c'}, {cons, {quotation, List1}, {cons, {quotation, List2}, Rest}}}, Reset) \
  -> rewrite(Reset({cons, {quotation, myappend(List1, List2)}, Rest}));
match({cons,{word, 'd'}, {cons, A, Rest}}, Reset) -> 
   rewrite(Reset({cons, A, {cons, A, Rest}}));
match({cons,{word, 'r'}, {cons, _, Rest}}, Reset) -> 
  rewrite(Reset(Rest));
match({cons,{word, 's'}, {cons, A, {cons, B, Rest}}}, Reset) -> 
  rewrite(Reset({cons, B, {cons, A, Rest}}));
match({cons, Head, Rest}, Reset) -> match(Rest, fun(X) -> Reset({cons, Head, X}) end);
match(nil, Reset) -> print_terms(Reset(nil)).
print_term({word, Atom}) -> atom_to_list(Atom);
print_term({quotation, List}) -> \"(\" ++ print_terms(List) ++ \")\".
print_terms(nil) -> \" \";
print_terms({cons,Term,List}) -> print_term(Term) ++ \" \" ++ print_terms(List). 
rewrite(List) -> match(List, fun(X) -> X end).";

fn translate_term(term:Term) -> String {
  match term {
    | Term::Word(s) => format!("{{word, '{}'}}", s),
    | Term::Quote(terms) => format!("{{quotation, {}}}", translate_terms(terms, "nil")),
  }
}

fn translate_terms(terms:Vec<Term>, nil:&str) -> String {
  let mut s = String::from(nil);
  for term in terms.into_iter().rev() {
    s = format!("{{cons, {}, {}}}", translate_term(term), s);
  }
  s
}

fn parser<'a>() -> impl Parser<&'a str, Output=Vec<Term>> {
  repeat::skip_until(char::char('"')).with(between(char::char('"'), char::char('"'), repeat::take_until(char::char('"')).flat_map(|s: String| crate::parser::terms_parser::<&str>().parse(s.as_ref()).map(|t| t.0.into_iter().rev().collect::<Vec<_>>()))))
}

/// # Panics
///
/// Will panic if the rules file could not be initialized or there was an error
/// sending the result
pub async fn run(rules:Vec<Rule>, sender:UnboundedSender<Vec<Term>>,
                 mut receiver:UnboundedReceiver<Vec<Term>>) {
  // write rules to file
  let mut string = BEFORE.to_string();
  for rule in rules {
    string.push_str(&format!("match({},Reset) -> rewrite(Reset({}));\n",
                             translate_terms(rule.pat, "Rest"),
                             &translate_terms(rule.rep, "Rest")));
  }
  string.push_str(AFTER);
  fs::write("./mlatu.erl", string).await.expect("could not write to file");
  if let Ok(mut child) = Command::new("erl").stdout(Stdio::piped()).stdin(Stdio::piped()).spawn() {
    // set up stdin and stdout, getting unused output out of the way
    let mut stdin = child.stdin.take().expect("child did not have a handle to stdin");
    let stdout = child.stdout.take().expect("child did not have a handle to stdout");
    let mut reader = BufReader::new(stdout).lines();
    std::mem::drop(reader.next_line()
                         .await
                         .expect("erlang process aborted unexpectedly")
                         .expect("no content in version string"));
    stdin.write_all(b"c(\"mlatu\").\n").await.expect("could not write to stdin");
    std::mem::drop(reader.next_line()
                         .await
                         .expect("erlang process aborted unexpectedly")
                         .expect("compilation of rules file failed"));

    let mut map:HashMap<Vec<Term>, Vec<Term>> = HashMap::new();
    while let Some(before) = receiver.recv().await {
      if let Some(after) = map.get(&before).cloned() {
        sender.send(after).expect("send error");
      } else {
        let term = translate_terms(before.clone().into_iter().rev().collect(), "nil");
        stdin.write_all(format!("mlatu:rewrite({}).\n", term).as_bytes())
             .await
             .expect("could not write to stdin");
        if let Ok(Some(line)) = reader.next_line().await {
          let after =
            parser().parse(&line)
                    .unwrap_or_else(|e| panic!("parsing failed: {} (line was \"{}\")", e, line))
                    .0;
          map.insert(before.clone(), after.clone());
          sender.send(after.clone()).expect("send error");
        }
      }
    }
    child.wait().await.expect("child could not be waited upon");
  } else {
    panic!("Could not spawn 'erl' process")
  }
}
