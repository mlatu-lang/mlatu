use std::path::PathBuf;

use mlatu::prolog::codegen;
use mlatu::prolog::util::{AssertLocation, ContextExt};
use mlatu::{parse_rules, prolog, Interactive};
use tokio::fs::OpenOptions;
use tokio::io::AsyncReadExt;
use tokio::sync::mpsc::unbounded_channel;

#[tokio::main]
async fn main() -> std::io::Result<()> {
  let path = if let Some(filename) = std::env::args().nth(2) {
    PathBuf::from(filename)
  } else {
    PathBuf::from("./prelude.mlt")
  };

  let mut file = OpenOptions::new().read(true).write(true).create(true).open(path).await?;
  let mut s = String::new();
  let _ = file.read_to_string(&mut s).await?;
  let rules = match parse_rules(&s) {
    | Ok(rules) => rules,
    | Err(error) => {
      eprintln!("{}", error);
      return Ok(())
    },
  };

  let (prolog_tx, mut interactive_rx) = unbounded_channel();
  let (interactive_tx, prolog_rx) = unbounded_channel();

  std::thread::spawn(move || {
    prolog::thread(|ctx, module| {
                     let clauses = codegen::generate(ctx, &rules).unwrap();
                     clauses.into_iter()
                            .try_for_each(|clause| {
                              ctx.assert(&clause.clause, Some(module), AssertLocation::Last)
                            })
                            .unwrap();
                   },
                   &prolog_tx,
                   prolog_rx)
  });

  let mut interactive = Interactive::new(interactive_tx)?;
  interactive.run(&mut interactive_rx).await;
  Ok(())
}
