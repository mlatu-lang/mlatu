use std::io;
use std::process::{Command, Stdio};

use combine::parser::char::char;
use combine::parser::repeat::take_until;
use combine::{eof, many1, satisfy, sep_by, EasyParser, Parser, Stream};

use crate::ast::Term;
use crate::gen::generate_query;

fn quote<Input>() -> impl Parser<Input, Output=char>
  where Input: Stream<Token=char>, {
  char('\'')
}

fn quoted_atom<Input>() -> impl Parser<Input, Output=String>
  where Input: Stream<Token=char>, {
  quote().with(take_until(quote())).skip(quote())
}

fn unquoted_atom<Input>() -> impl Parser<Input, Output=String>
  where Input: Stream<Token=char>, {
  many1::<String, _, _>(satisfy(|c| c != ',' && c != ']'))
}

fn atom<Input>() -> impl Parser<Input, Output=String>
  where Input: Stream<Token=char>, {
  quoted_atom().or(unquoted_atom())
}

fn parser<Input>() -> impl Parser<Input, Output=Vec<String>>
  where Input: Stream<Token=char>, {
  char('[').with(sep_by(atom(), char(','))).skip(char(']')).skip(eof())
}

/// # Errors
///
/// Returns `Err` if failed to execute 'swipl' child or failed to wait on child.
///
/// # Panics
///
/// Panics if parsing the output of the goal fails.
pub fn execute(path:&str, terms:&[Term]) -> io::Result<Vec<String>> {
  let query = generate_query(terms);
  let child = Command::new("swipl").args(["-g", &query, "-t", "halt", path])
                                   .stdin(Stdio::piped())
                                   .stdout(Stdio::piped())
                                   .stderr(Stdio::piped())
                                   .spawn()?;
  let output = child.wait_with_output()?;
  let cow = String::from_utf8_lossy(&output.stdout);
  let mut parser = parser();
  let (result, _) = parser.easy_parse(cow.as_ref()).unwrap();
  Ok(result)
}
