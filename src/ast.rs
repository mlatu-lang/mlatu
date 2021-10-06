use std::fmt;
use std::sync::Arc;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
pub enum Term {
  Word(Arc<String>),
  Quote(Vec<Self>),
}

impl Term {
  #[must_use]
  pub fn new_word(name:impl Into<String>) -> Self { Self::Word(Arc::new(name.into())) }

  #[must_use]
  pub fn new_quote(terms:Vec<Self>) -> Self { Self::Quote(terms) }
}

impl fmt::Display for Term {
  fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      | Self::Word(name) => write!(f, "{}", name.as_ref()),
      | Self::Quote(terms) => {
        write!(f, "(")?;
        for term in terms {
          write!(f, " {}", term)?;
        }
        write!(f, " )")?;
        Ok(())
      },
    }
  }
}

pub type Rule = (Vec<Term>, Vec<Term>);

pub fn pretty_rule(pattern:&[Term], replacement:&[Term], s:&mut String) {
  for term in pattern {
    s.push_str(&term.to_string());
    s.push(' ');
  }
  s.push('=');
  for term in replacement {
    s.push_str(&term.to_string());
  }
  s.push(';');
}

mod prolog {
  use super::Term as AstTerm;
  use crate::prolog::{attempt_opt, term_getable, PrologError, Term, TermGetable};

  term_getable! {
    (AstTerm, term) => {
      match term.get::<Vec<Self>>() {
        Ok(terms) => {
          let quote = terms.into_iter().rev().collect();
          Some(Self::new_quote(quote))
        },
        Err(PrologError::Exception) => None,
        Err(PrologError::Failure) => {
          attempt_opt(term.get_atom_name(|x| x.map(Self::new_word))).unwrap_or(None).flatten()
        }
      }
    }
  }
}
