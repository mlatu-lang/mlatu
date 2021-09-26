use std::fs::File;
use std::io::Write;
use std::path::Path;

use crate::ast::{Rule, Term};

const BUILTIN:&[u8] = b"
equiv(['dup'|[X|Xs]], Other) :- equiv([X|[X|Xs]], Other).
equiv(['pop'|[_|Xs]], Other) :- equiv(Xs, Other).
equiv(['i'|[Q|Xs]], Other) :- append(Q, Xs, NewXs), equiv(NewXs, Other).
equiv(['cons'|[Q|[X|Xs]]], Other) :- equiv([[X|Q]|Xs], Other).
equiv(['dip'|[Q|[X|Xs]]], Other) :- append(Q, Xs, NewXs), equiv([X|NewXs], Other).
equiv([], []).
equiv([X|Xs], Other) :- equiv(Xs, New), (dif(Xs, New) -> equiv([X|New], Other) ; [X|Xs] = Other).
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
  let mut s = ("equiv([").to_string();
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
pub fn generate<P:AsRef<Path>>(rules:Vec<Rule>, path:P) -> std::io::Result<()> {
  let mut file = File::create(path)?;
  for rule in rules {
    let mut pattern = String::from("Rest");
    for elem in &rule.pattern {
      pattern.insert_str(0, &format!("[{}|", transform_term(elem)));
      pattern.push(']');
    }
    let mut replacement = String::from("Rest");
    for elem in &rule.replacement {
      replacement.insert_str(0, &format!("[{}|", transform_term(elem)));
      replacement.push(']');
    }
    file.write_all(format!("equiv({}, Other) :- equiv({}, Other).\n", pattern, replacement).as_bytes())?;
  }
  file.write_all(BUILTIN)?;
  Ok(())
}
