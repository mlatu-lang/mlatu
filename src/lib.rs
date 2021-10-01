#![feature(stdio_locked)]
#![deny(clippy::correctness)]
#![warn(clippy::suspicious,
        clippy::style,
        clippy::complexity,
        clippy::perf,
        clippy::pedantic,
        clippy::cargo,
        clippy::nursery,
        clippy::as_conversions,
        clippy::clone_on_ref_ptr,
        clippy::create_dir,
        clippy::decimal_literal_representation,
        clippy::exit,
        clippy::filetype_is_file,
        clippy::float_cmp_const,
        clippy::if_then_some_else_none,
        clippy::lossy_float_literal,
        clippy::mem_forget,
        clippy::mod_module_files,
        clippy::rc_buffer,
        clippy::rc_mutex,
        clippy::unwrap_used,
        clippy::verbose_file_reads)]

mod ast;
mod editor;
mod execute;
mod gen;
mod parser;
mod terminal;

pub use ast::{Rule, Term};
pub use editor::Editor;
pub use execute::execute;
pub use gen::generate;
pub use parser::{parse_rule, parse_rules, parse_term, parse_terms};
