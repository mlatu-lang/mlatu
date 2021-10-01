use async_std::fs::OpenOptions;
use async_std::path::PathBuf;
use async_std::prelude::*;
use mlatu::{parse_rules, Editor};

#[async_std::main]
async fn main() -> std::io::Result<()> {
  let path = if let Some(filename) = std::env::args().nth(2) {
    PathBuf::from(filename)
  } else {
    PathBuf::from("./prelude.mlt")
  };
  let mut file = OpenOptions::new().read(true).write(true).create(true).open(path).await?;
  let mut s = String::new();
  let _ = file.read_to_string(&mut s).await?;
  match parse_rules(&s) {
    | Ok(rules) => Editor::new(file, rules).unwrap().run().await,
    | Err(error) => eprintln!("{}", error),
  }
  Ok(())
}
