use std::fmt;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
pub enum Term {
  Word(String),
  Quote(Vec<Self>),
}

impl Term {
  #[must_use]
  pub fn new_word(name:impl Into<String>) -> Self { Self::Word(name.into()) }

  #[must_use]
  pub fn new_quote(terms:Vec<Self>) -> Self { Self::Quote(terms) }
}

impl fmt::Display for Term {
  fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      | Self::Word(name) => write!(f, "{}", name),
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

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
pub struct Rule {
  pub pat:Vec<Term>,
  pub rep:Vec<Term>,
}

pub mod binary {
  use super::{Rule, Term};

  fn add_length(other:&[u8], buf:&mut Vec<u8>) {
    let other_len = u32::try_from(other.len()).expect("'other.len()' is greater than u32::MAX");
    buf.extend_from_slice(&other_len.to_be_bytes());
    buf.extend_from_slice(other);
  }

  fn serialize_term(term:Term, buf:&mut Vec<u8>) {
    match term {
      | Term::Word(name) => {
        buf.push(0_u8);
        add_length(name.as_bytes(), buf);
      },
      | Term::Quote(terms) => {
        buf.push(1_u8);
        let mut terms_bytes = Vec::new();
        for term in terms {
          serialize_term(term, &mut terms_bytes);
        }
        add_length(&terms_bytes, buf);
      },
    }
  }

  #[must_use]
  pub fn serialize_rules(rules:Vec<Rule>) -> Vec<u8> {
    let mut buf = Vec::new();
    let mut rules_bytes = Vec::new();
    for rule in rules {
      let mut pattern_bytes = Vec::new();
      for term in rule.pat {
        serialize_term(term, &mut pattern_bytes);
      }
      add_length(&pattern_bytes, &mut rules_bytes);
      let mut replacement_bytes = Vec::new();
      for term in rule.rep {
        serialize_term(term, &mut replacement_bytes);
      }
      add_length(&replacement_bytes, &mut rules_bytes);
    }
    add_length(&rules_bytes, &mut buf);
    buf
  }

  fn get_length(buf:&mut Vec<u8>) -> Vec<u8> {
    let slice_len = usize::try_from(u32::from_be_bytes(buf[..4].try_into().expect("'buf' has less than four elements"))).expect("encoded u32 does not fit into usize");
    let vec = Vec::from(&buf[4..(4 + slice_len)]);
    *buf = buf[(4 + slice_len)..].to_vec();
    vec
  }

  fn deserialize_term(buf:&mut Vec<u8>) -> Option<Term> {
    let first = buf[0];
    *buf = buf[1..].to_vec();
    match first {
      | 0_u8 => {
        let name_bytes = get_length(buf);
        String::from_utf8(name_bytes).ok().map(Term::new_word)
      },
      | 1_u8 => {
        let mut terms_bytes = get_length(buf);
        let mut terms = Vec::new();
        while !terms_bytes.is_empty() {
          match deserialize_term(&mut terms_bytes) {
            | Some(term) => terms.push(term),
            | None => return None,
          }
        }
        Some(Term::Quote(terms))
      },
      | _ => None,
    }
  }

  pub fn deserialize_rules(buf:&mut Vec<u8>) -> Option<Vec<Rule>> {
    println!("len: {}", buf.len());
    let mut rules_bytes = get_length(buf);
    let mut rules = Vec::new();
    while !rules_bytes.is_empty() {
      let mut pattern_bytes = get_length(&mut rules_bytes);
      let mut pat = Vec::new();
      while !pattern_bytes.is_empty() {
        match deserialize_term(&mut pattern_bytes) {
          | Some(term) => pat.push(term),
          | None => return None,
        }
      }
      let mut replacement_bytes = get_length(&mut rules_bytes);
      let mut rep = Vec::new();
      while !replacement_bytes.is_empty() {
        match deserialize_term(&mut replacement_bytes) {
          | Some(term) => rep.push(term),
          | None => return None,
        }
      }
      rules.push(Rule { pat, rep });
    }
    Some(rules)
  }
}
