use async_std::fs::OpenOptions;
use async_std::io;
use async_std::io::prelude::*;
use async_std::path::PathBuf;
use mlatu::{parse_rules, parse_terms, prolog};
use prolog::util::AssertLocation;
use prolog::{codegen, ContextExt};

// TODO: actual CLI, better REPL (w/ rustyline)
// TODO: Bring back structural editing
// BUG: Ctrl-C forwards to SWIPl
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

  let clauses = match codegen::generate(&ctx, &rules) {
    | Ok(clauses) => clauses,
    | Err(error) => {
      eprintln!("Error while compiling rules: {}", error);
      return Ok(())
    },
  };

  if let Err(error) =
    clauses.into_iter()
           .try_for_each(|clause| ctx.assert(&clause.clause, Some(&module), AssertLocation::Last))
  {
    eprintln!("Error while compiling rules: {}", error);
    return Ok(())
  };

  let stdin = io::stdin();
  let mut stdout = io::stdout();

  loop {
    let mut contents = String::new();
    print!(">>> ");
    stdout.flush().await?;
    stdin.read_line(&mut contents).await?;

    if contents.trim() == "exit" || contents.trim() == "quit" || contents.is_empty() {
      println!("Goodbye!");
      break Ok(())
    }

    let terms = match parse_terms(&contents) {
      | Ok(terms) => terms,
      | Err(error) => {
        eprintln!("{}", error);
        continue
      },
    };

    let (list, other) = match codegen::generate_query(&ctx, &terms) {
      | Ok(q) => q,
      | Err(error) => {
        eprintln!("Error while compiling query: {}", error);
        continue
      },
    };

    if let Err(error) = ctx.call_once(prolog::pred!(mlatu: rewrite / 2), [&list, &other]) {
      eprintln!("Error while executing query: {}", error);
      continue
    }
    // TODO: proper printing
    let canon = match ctx.canonical(&other) {
      | Some(s) => s,
      | None => {
        eprintln!("Could not format output");
        continue
      },
    };

    println!("==> {}", canon);
  }
}
