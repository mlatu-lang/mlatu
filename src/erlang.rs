use std::collections::HashMap;
use std::process::Stdio;

use combine::parser::{char, repeat};
use combine::{between, Parser};
use tokio::fs;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::Command;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};

use crate::ast::{Rule, Term};

pub const BEFORE:&str = "-module(mlatu). -export([interact/1]). 
driver([], Prefix) -> Prefix;
driver([Head|Tail], Prefix) ->
  case find_redex([Head|Tail]) of  
    {redex_found, New} -> driver(lists:reverse(Prefix, New), []); 
    {redex_not_found} -> driver(Tail, [Head|Prefix])
end.
find_redex(List) -> case List of \n";

pub const AFTER:&str = "
  [{word, 'u'}, {quotation, A} | Rest] -> {redex_found, A ++ Rest};
  [{word, 'q'}, A | Rest] -> {redex_found, [{quotation, [A]} | Rest]};
  [{word, 'c'}, {quotation, List1}, {quotation, List2} | Rest] -> 
   {redex_found, [{quotation, List1 ++ List2} | Rest]};
  [{word, 'd'}, A | Rest] -> {redex_found, [A, A | Rest]};
  [{word, 'r'}, _ | Rest] -> {redex_found, Rest};
  [{word, 's'}, A, B | Rest] -> {redex_found, [B, A | Rest]};
  _ -> {redex_not_found}
end.
print_term({word, Atom}) -> atom_to_list(Atom);
print_term({quotation, List}) -> \"(\" ++ print_terms(List) ++ \")\".
print_terms([]) -> \" \";
print_terms([Term|List]) -> print_term(Term) ++ \" \" ++ print_terms(List). 
interact(List) -> print_terms(driver(List, [])).";

fn translate_term(term:Term) -> String {
  match term {
    | Term::Word(s) => format!("{{word, '{}'}}", s),
    | Term::Quote(terms) => format!("{{quotation, {}}}", translate_terms(terms, "[]")),
  }
}

fn translate_terms(terms:Vec<Term>, end:&str) -> String {
  if terms.is_empty() {
    end.to_string()
  } else {
    let mut s = "[".to_owned();
    let mut iter = terms.into_iter();
    if let Some(term) = iter.next() {
      s.push_str(&translate_term(term));
    }
    for term in iter {
      s.push(',');
      s.push_str(&translate_term(term.clone()));
    }
    s.push('|');
    s.push_str(end);
    s.push(']');
    s
  }
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
    string.push_str(&translate_terms(rule.pat, "Rest"));
    string.push_str(" -> {redex_found,");
    string.push_str(&translate_terms(rule.rep, "Rest"));
    string.push_str("};\n");
  }
  string.push_str(AFTER);
  fs::write("./mlatu.erl", string).await.expect("could not write to file");
  if let Ok(mut child) = Command::new("erl").stdout(Stdio::piped()).stdin(Stdio::piped()).spawn() {
    // set up stdin and stdout, getting unused output out of the way
    let mut stdin = child.stdin.take().expect("child did not have a handle to stdin");
    let stdout = child.stdout.take().expect("child did not have a handle to stdout");
    let mut reader = BufReader::new(stdout).lines();
    assert_eq!(reader.next_line().await.expect("erlang process aborted unexpectedly"),
               Some("Eshell V12.1.2  (abort with ^G)".to_string()));
    stdin.write_all(b"c(\"mlatu\").\n").await.expect("could not write to stdin");
    assert_eq!(reader.next_line().await.expect("erlang process aborted unexpectedly"),
               Some("1> {ok,mlatu}".to_string()));

    let mut map:HashMap<Vec<Term>, Vec<Term>> = HashMap::new();
    while let Some(before) = receiver.recv().await {
      if let Some(after) = map.get(&before).cloned() {
        sender.send(after).expect("send error");
      } else {
        let term = translate_terms(before.clone(), "[]");
        stdin.write_all(format!("mlatu:interact({}).\n", term).as_bytes())
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
