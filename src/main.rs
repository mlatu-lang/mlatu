use async_std::fs::OpenOptions;
use async_std::io::prelude::*;
use async_std::path::PathBuf;
use mlatu::{parse_rules, prolog, Interactive};
use prolog::codegen;
use prolog::util::{AssertLocation, ContextExt};

#[async_std::main]
async fn main() -> std::io::Result<()> {
  let engine = mlatu::prolog::init_engine();
  let ctx:prolog::Context<'_, _> = engine.activate().into();

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

  let module = prolog::Module::new("mlatu");

  let clauses = codegen::generate(&ctx, &rules).unwrap();
  clauses.into_iter()
         .try_for_each(|clause| ctx.assert(&clause.clause, Some(&module), AssertLocation::Last))
         .unwrap();

  let (mut interactive, mut channel) = Interactive::new(rules, ctx)?;
  interactive.run(channel).await;
  Ok(())
}
