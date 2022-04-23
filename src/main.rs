#![feature(with_options)]

use std::path::{Path, PathBuf};

use clap::{arg, command, Command};
use im::Vector;
use mlatu::{Editor, Interactive};
use mlatu_lib::{parse, Engine, Rule};

fn load_file(engine:&Engine, filename:&str) -> Result<Vector<Rule>, String> {
  let path = Path::new(&filename);
  match std::fs::read(path) {
    | Ok(bytes) => match String::from_utf8(bytes) {
      | Ok(string) => match parse::rules(engine, &string) {
        | Ok(rules) => Ok(rules),
        | Err(e) => Err(format!("Error while parsing '{}': {}", filename, e)),
      },
      | Err(e) => Err(format!("Error in decoding '{}': {}", filename, e)),
    },
    | Err(e) => Err(format!("Error while reading '{}': {}", filename, e)),
  }
}

fn load_files(engine:&Engine, files:Vec<String>) -> Result<Vector<Rule>, String> {
  let mut rules = Vector::new();
  for file in files {
    rules.extend(load_file(engine, &file)?);
  }
  Ok(rules)
}

#[tokio::main]
async fn main() -> Result<(), String> {
  let matches = command!().propagate_version(true)
                          .arg(arg!([FILES]).multiple_values(true).help("Rule files to use"))
                          .subcommand(Command::new("edit").about("the structured editor")
                                                          .arg(arg!([FILE]).help("Rule file to \
                                                                                  edit")))
                          .get_matches();

  let engine = Engine::new();

  match matches.subcommand() {
    | Some(("edit", sub_matches)) => {
      let filename = sub_matches.value_of("FILE").unwrap();
      let rules = load_file(&engine, filename)?;
      let mut path = PathBuf::from(filename);
      let _ = path.set_extension("mlb");
      match path.canonicalize() {
        | Ok(path) => Editor::new(engine, path, rules)?.run().await,
        | Err(_) => eprintln!("Path could not be canonicalized"),
      }
    },
    | _ => {
      let mut files = Vec::new();
      if let Some(args) = matches.values_of("FILES") {
        files.extend(args.map(ToOwned::to_owned));
      }
      match load_files(&engine, files) {
        | Ok(rules) => match Interactive::new(engine, rules) {
          | Ok(mut interactive) => match interactive.run().await {
            | Ok(()) => {},
            | Err(error) => eprintln!("{}", error),
          },
          | Err(error) => eprintln!("{}", error),
        },
        | Err(error) => eprintln!("{}", error),
      }
    },
  }

  Ok(())
}
