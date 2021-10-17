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

pub type Rule = (Vec<Term>, Vec<Term>);

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

pub mod serde {
  use nom::bytes::complete::{tag, take};
  use nom::multi::{count, many0};
  use nom::sequence::preceded;
  use nom::{IResult, Parser};

  use super::{Rule, Term};

  fn usize_to_bytes(len:usize) -> Vec<u8> { Vec::from(u32::try_from(len).unwrap().to_be_bytes()) }

  fn serialize_term(term:Term) -> Vec<u8> {
    match term {
      | Term::Word(s) => {
        let mut bytes = vec![0x00];
        let slice = s.as_bytes();
        bytes.extend(usize_to_bytes(slice.len()));
        bytes.extend_from_slice(slice);
        bytes
      },
      | Term::Quote(terms) => {
        let mut bytes = vec![0x01];
        bytes.extend(serialize_terms(terms));
        bytes
      },
    }
  }

  fn serialize_terms(terms:Vec<Term>) -> Vec<u8> {
    let rest:Vec<u8> = terms.into_iter().flat_map(|term| serialize_term(term)).collect();
    let mut bytes = usize_to_bytes(rest.len());
    bytes.extend(rest);
    bytes
  }

  fn serialize_rule(rule:Rule) -> Vec<u8> {
    let mut bytes = serialize_terms(rule.0);
    bytes.extend(serialize_terms(rule.1));
    bytes
  }

  pub fn serialize_rules(rules:Vec<Rule>) -> Vec<u8> {
    rules.into_iter().flat_map(|rule| serialize_rule(rule)).collect()
  }

  fn word_parser(bytes:&[u8]) -> IResult<&[u8], Term> {
    preceded(tag(&[0x00]),
             take(4usize).map(|bs:&[u8]| u32::from_be_bytes(bs.try_into().unwrap()))
                         .flat_map(|len| {
                           take(len).map(|bs:&[u8]| {
                                      Term::new_word(String::from_utf8(bs.into()).unwrap())
                                    })
                         }))(bytes)
  }

  fn quotation_parser(bytes:&[u8]) -> IResult<&[u8], Term> {
    preceded(tag(&[0x01]), terms_parser.map(Term::new_quote))(bytes)
  }

  fn term_parser(bytes:&[u8]) -> IResult<&[u8], Term> {
    word_parser.or(quotation_parser).parse(bytes)
  }

  fn terms_parser(bytes:&[u8]) -> IResult<&[u8], Vec<Term>> {
    take(4usize).map(|bs:&[u8]| u32::from_be_bytes(bs.try_into().unwrap()))
                .flat_map(|len| count(term_parser, len.try_into().unwrap()))
                .parse(bytes)
  }

  fn rule_parser(bytes:&[u8]) -> IResult<&[u8], Rule> {
    terms_parser.and(terms_parser).parse(bytes)
  }

  fn rules_parser(bytes:&[u8]) -> IResult<&[u8], Vec<Rule>> { many0(rule_parser).parse(bytes) }

  pub fn deserialize_rules(bytes:&[u8]) -> Result<Vec<Rule>, String> {
    rules_parser(bytes).map(|res| res.1).map_err(|e| e.to_string())
  }
}
