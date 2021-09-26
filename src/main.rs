use mlatu::{execute, generate, parse_rules, parse_terms};

fn main() {
  let input = "1 2 + = 3 hi ;";
  match parse_rules(input) {
    | Ok(rules) => {
      generate(rules, "equiv.pl").unwrap();
      let terms = &execute("equiv.pl", &parse_terms("1 2 +").unwrap()).unwrap();
      for term in terms.iter().rev() {
        print!("{} ", term);
      }
      println!();
    },
    | Err(s) => println!("Err: {}", s),
  }
}
