use anyhow::Context;
use async_std::process::{Command, Output, Stdio};
use combine::parser::byte::byte;
use combine::parser::repeat::take_until;
use combine::{eof, many1, satisfy, sep_by, Parser, Stream};

use crate::ast::Term;
use crate::gen::generate_query;

fn quote<Input>() -> impl Parser<Input, Output=u8>
  where Input: Stream<Token=u8>, {
  byte(b'\'')
}

fn quoted_atom<Input>() -> impl Parser<Input, Output=String>
  where Input: Stream<Token=u8>, {
  quote().with(take_until::<Vec<_>, _, _>(quote()))
         .skip(quote())
         .map(|v| String::from_utf8_lossy(&v).into_owned())
}

fn unquoted_atom<Input>() -> impl Parser<Input, Output=String>
  where Input: Stream<Token=u8>, {
  many1::<Vec<_>, _, _>(satisfy(|c| c != b',' && c != b']')).map(|v| String::from_utf8_lossy(&v).into_owned())
}

fn atom<Input>() -> impl Parser<Input, Output=String>
  where Input: Stream<Token=u8>, {
  quoted_atom().or(unquoted_atom())
}

fn parser<Input>() -> impl Parser<Input, Output=Vec<String>>
  where Input: Stream<Token=u8>, {
  byte(b'[').with(sep_by(atom(), byte(b','))).skip(byte(b']')).skip(eof())
}

/// # Errors
///
/// Returns `Err` if failed to execute 'swipl' child or failed to wait on child.
///
/// # Panics
///
/// Panics if parsing the output of the goal fails.
pub async fn execute(path:&str, terms:&[Term]) -> anyhow::Result<Vec<String>> {
  let query = generate_query(terms);
  let Output { stdout, status: _, stderr: _, } =
    Command::new("swipl").args(["-g", &query, "-t", "halt", path])
                         .stdin(Stdio::piped())
                         .stdout(Stdio::piped())
                         .stderr(Stdio::piped())
                         .output()
                         .await
                         .context("Failed to spawn 'swipl' and capture its output")?;
  let mut parser = parser();
  let (result, _) = parser.parse(stdout.as_slice()).context("Failed to parse 'swipl's output")?;
  Ok(result)
}
