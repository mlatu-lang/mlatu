use clap::{App, Arg, SubCommand};
use mlatu::prolog::codegen;
use mlatu::prolog::util::{AssertLocation, ContextExt};
use mlatu::{parse_rules, prolog, Editor, Interactive, Rule};
use tokio::fs::{read_to_string, File};
use tokio::sync::mpsc::unbounded_channel;

async fn load_files(files:Vec<String>) -> Result<Vec<Rule>, String> {
  let mut rules = Vec::new();
  for file in files {
    match read_to_string(file).await {
      | Ok(string) => match parse_rules(&string) {
        | Ok(rs) => rules.extend(rs),
        | Err(error) => return Err(error),
      },
      | Err(error) => return Err(error.to_string()),
    }
  }
  Ok(rules)
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
  let matches = App::new("mlatu")
                 .version("0.1")
                 .author("Caden Haustein <code@brightlysalty.33mail.com>")
                 .about("the mlatu language interface")
                 .arg(Arg::with_name("FILES").multiple(true).help("Sets the rule files to use")).
                  arg(Arg::with_name("no-prelude").long("no-prelude").help("Doesn't load the normal prelude"))
                  .subcommand(SubCommand::with_name("edit")
                    .about("the mlatu structured editor")
                    .version("0.1")
                    .author("Caden Haustein <code@brightlysalty.33mail.com>")
                    .arg(Arg::with_name("FILE").required(true).help("Sets the rule file to edit")
                  ))
                .get_matches();
  let mut files = Vec::new();
  if !matches.is_present("no-prelude") {
    files.push("./prelude.mlt".to_string());
  }
  if let Some(args) = matches.values_of("FILES") {
    files.extend(args.map(ToOwned::to_owned));
  }
  match load_files(files).await {
    | Ok(rules) => match matches.subcommand() {
      | ("edit", Some(sub_matches)) => {
        let file = File::create(sub_matches.value_of("FILE").unwrap()).await?;
        Editor::new(file, rules)?.run().await;
      },
      | _ => {
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

        Interactive::new(interactive_tx)?.run(&mut interactive_rx).await;
      },
    },
    | Err(error) => eprintln!("Error while reading rule files: {}", error),
  }

  Ok(())
}
