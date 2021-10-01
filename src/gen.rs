use async_std::fs::File;
use async_std::io::WriteExt;
use async_std::path::PathBuf;

use crate::ast::{Rule, Term};

const BUILTIN:&[u8] = b"
equiv(['dup',X|Xs], Other) :- equiv([X,X|Xs], Other).
equiv(['pop',X|Xs], Other) :- equiv(Xs, Other).
equiv(['i',[]|Xs], Other) :- equiv(Xs, Other).
equiv(['i',[H|T]|Xs], Other) :- append([H|T], Xs, NewXs), equiv(NewXs, Other).
equiv(['cons',[],X|Xs], Other) :- equiv([[X]|Xs], Other).
equiv(['cons',[H|T],X|Xs], Other) :- equiv([[X,H|T]|Xs], Other).
equiv(['dip',[],X|Xs], Other) :- equiv([X|Xs], Other).
equiv(['dip',[H|T],X|Xs], Other) :- append([H|T], Xs, NewXs), equiv([X|NewXs], Other).
equiv([], []).
equiv([X|Xs], Other) :- equiv(Xs, New), (Xs \\= New -> equiv([X|New], Other) ; Other = [X|Xs]).
rewrite(List, Other) :- equiv(List, Other), List \\= Other.
";

fn transform_term(term:&Term) -> String {
  match term {
    | Term::Word(s) => format!("'{}'", s),
    | Term::Quote(terms) =>
      if terms.is_empty() {
        ("[]").to_string()
      } else {
        let mut s = ("[]").to_string();
        for term in terms.iter().rev() {
          s.insert_str(0, &format!("[{}|", transform_term(term)));
          s.push(']');
        }
        s
      },
  }
}

pub fn generate_query(terms:&[Term]) -> String {
  let mut s = ("rewrite([").to_string();
  let mut iter = terms.iter().rev();
  if let Some(first) = iter.next() {
    s.push_str(&transform_term(first));
  }
  for term in iter {
    s.push_str(&format!(", {}", transform_term(term)));
  }
  s.push_str("], Other), write_canonical(Other).");
  s
}

/// # Errors
///
/// Returns `Err` if there was an error creating the file or writing to it.
pub async fn generate(rules:Vec<Rule>, path:PathBuf) -> std::io::Result<()> {
  let mut file = File::create(path).await?;
  for rule in rules {
    let mut s = "equiv([".to_owned();
    let mut pattern_iter = rule.pattern.iter().rev();
    if let Some(first) = pattern_iter.next() {
      s.push_str(&transform_term(first));
    }
    for term in pattern_iter {
      s.push_str(&format!(", {}", transform_term(term)));
    }
    s.push_str("|Rest], Other) :- equiv([");
    let mut replacement_iter = rule.replacement.iter().rev();
    if let Some(first) = replacement_iter.next() {
      s.push_str(&transform_term(first));
    }
    for term in replacement_iter {
      s.push_str(&format!(", {}", transform_term(term)));
    }
    s.push_str("|Rest], Other).\n");
    file.write_all(s.as_bytes()).await?;
  }
  file.write_all(BUILTIN).await?;
  Ok(())
}
