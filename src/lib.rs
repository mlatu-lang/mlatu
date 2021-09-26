#![deny(clippy::correctness)]
#![warn(clippy::suspicious, clippy::style, clippy::complexity, clippy::perf, clippy::pedantic, clippy::cargo)]

mod ast;
mod execute;
mod gen;
mod parser;

pub use ast::{Rule, Term};
pub use execute::execute;
pub use gen::generate;
pub use parser::{parse_rule, parse_rules, parse_term, parse_terms};
