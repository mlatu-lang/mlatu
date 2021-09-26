use std::fmt;
use std::sync::Arc;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
pub enum Term {
  Word(Arc<String>),
  Quote(Vec<Term>),
}

impl Term {
  #[must_use]
  pub fn new_word(name:impl Into<String>) -> Self { Self::Word(Arc::new(name.into())) }

  #[must_use]
  pub fn new_quote(terms:Vec<Term>) -> Self { Self::Quote(terms) }
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
        write!(f, ")")?;
        Ok(())
      },
    }
  }
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
pub struct Rule {
  pub pattern:Vec<Term>,
  pub replacement:Vec<Term>,
}

impl Rule {
  #[must_use]
  pub fn new(pattern:Vec<Term>, replacement:Vec<Term>) -> Self { Self { pattern, replacement } }
}

impl fmt::Display for Rule {
  fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
    for term in &self.pattern {
      write!(f, "{} ", term)?;
    }
    write!(f, "->")?;
    for term in &self.replacement {
      write!(f, " {}", term)?;
    }
    write!(f, ";")
  }
}
