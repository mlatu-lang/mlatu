use std::process::Stdio;

use combine::parser::{char, repeat};
use combine::{between, Parser};
use tokio::fs;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::Command;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};

use crate::ast::{Rule, Term};

pub const BEFORE:&str = "-module(mlatu). -export([interact/1]). 
spawn_rewrite(List, IfOk, IfNone) -> Self = self(), spawn(fun() -> inner_rewrite(List, Self) end), receive {ok, NewRest} -> IfOk(NewRest); {none} -> IfNone() end.
no_rewrite(Pid) -> Pid ! {none}.
rewrite_to(Pid, New) -> Pid ! {ok, New}.
inner_rewrite(List, Parent) -> case List of ";

pub const AFTER:&str = "
  [] -> no_rewrite(Parent);
  [{word, u}, {quote, List} | Rest] -> rewrite_to(Parent, List ++ Rest);
  [{word, q}, A | Rest] -> rewrite_to(Parent, [{quote, [A]} | Rest]);
  [{word, c}, {quote, List1}, {quote, List2} | Rest] -> rewrite_to(Parent, [{quote, List1 ++  List2} | Rest]);
  [{word, d}, A | Rest] -> rewrite_to(Parent, [A, A | Rest]);
  [{word, r}, _ | Rest] -> rewrite_to(Parent, Rest);
  [{word, s}, A, B | Rest] -> rewrite_to(Parent, [B, A | Rest]);
  [First | Rest] -> spawn_rewrite(Rest, fun(NewRest) -> spawn(fun() -> inner_rewrite([First | NewRest], Parent) end) end, fun() -> no_rewrite(Parent) end)
    end.
print_term({word, Atom}) -> atom_to_list(Atom);
print_term({quote, List}) -> \"(\" ++ print_terms(List) ++ \")\".
print_terms(MTerms) -> string:trim(lists:foldl(fun(Term, S) -> print_term(Term) ++ \" \" ++ S end, \
                        \"\", MTerms)). 
interact(Original) ->
    spawn_rewrite(Original, fun(New) -> print_terms(New) end, fun() -> print_terms(Original) end).
";

fn translate_term(term:Term) -> String {
  match term {
    | Term::Word(s) => format!("{{word, '{}'}}", s),
    | Term::Quote(terms) => format!("{{quote, {}}}", translate_terms(terms, "")),
  }
}

fn translate_terms(terms:Vec<Term>, end:&str) -> String {
  let mut s = "[".to_owned();
  let mut iter = terms.into_iter().rev();
  if let Some(term) = iter.next() {
    s.push_str(&translate_term(term));
  }
  for term in iter {
    s.push(',');
    s.push_str(&translate_term(term.clone()));
  }
  s.push_str(end);
  s.push(']');
  s
}

fn parser<'a>() -> impl Parser<&'a str, Output=Vec<Term>> {
  char::string("[]").map(|_| vec![]).or(repeat::skip_until(char::char('"')).with(between(char::char('"'), char::char('"'), repeat::take_until(char::char('"')).flat_map(|s: String| crate::parser::terms_parser::<&str>().parse(s.as_ref()).map(|t| t.0)))))
}

/// # Panics
///
/// Will panic if the rules file could not be initialized or there was an error
/// sending the result
pub async fn run(rules:Vec<Rule>, sender:UnboundedSender<Vec<Term>>,
                 mut receiver:UnboundedReceiver<Vec<Term>>) {
  let mut string = BEFORE.to_string();
  for rule in rules {
    string.push_str(&translate_terms(rule.0, "|Rest"));
    string.push_str(" -> rewrite_to(Parent, ");
    string.push_str(&translate_terms(rule.1, "|Rest"));
    string.push_str(");");
  }
  string.push_str(AFTER);
  fs::write("./mlatu.erl", string).await.expect("could not write to file");
  if let Ok(mut child) = Command::new("erl").stdout(Stdio::piped()).stdin(Stdio::piped()).spawn() {
    let mut stdin = child.stdin.take().expect("child did not have a handle to stdin");
    let stdout = child.stdout.take().expect("child did not have a handle to stdout");
    let mut reader = BufReader::new(stdout).lines();
    assert_eq!(reader.next_line().await.unwrap(),
               Some("Eshell V12.1.2  (abort with ^G)".to_string()));
    stdin.write_all(b"c(\"mlatu\").\n").await.expect("could not write to stdin");
    assert_eq!(reader.next_line().await.unwrap(), Some("1> {ok,mlatu}".to_string()));
    while let Some(before) = receiver.recv().await {
      let term = translate_terms(before, "");
      stdin.write_all(format!("mlatu:interact({}).\n", term).as_bytes())
           .await
           .expect("could not write to stdin");
      if let Ok(Some(line)) = reader.next_line().await {
        let result = parser().parse(&line).expect("parsing failed");
        sender.send(result.0).expect("send error");
      }
    }
    child.wait().await.unwrap();
  } else {
    panic!("Could not spawn 'erl' process")
  }
}
