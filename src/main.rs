use mlatu::{execute, generate, parse_rules, parse_terms};

fn main() -> anyhow::Result<()> {
  let rules = parse_rules("1 2 + = 3 hi ;")?;
  generate(rules, "equiv.pl")?;
  let input_terms = parse_terms("1 2 +")?;
  let terms = execute("equiv.pl", &input_terms)?;
  for term in terms.iter().rev() {
    print!("{} ", term);
  }
  println!();
  Ok(())
}
