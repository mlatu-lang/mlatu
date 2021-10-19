use swipl::prelude::*;

use super::util::{Clause, ContextExt};
use crate::ast::{Rule, Term as AstTerm};

prolog! {
  #[module("mlatu")]
  pub fn rewrite(list, other);
}

fn transform_term<'a>(ctx:&'a Context<'a, ActivatedEngine<'a>>, term:&'a AstTerm)
                      -> PrologResult<Term<'a>> {
  match term {
    | AstTerm::Word(s) => term!(ctx: #Atom::new(s)),
    | AstTerm::Quote(terms) => {
      let terms =
        terms.iter().rev().map(|t| transform_term(ctx, t)).collect::<Result<Vec<_>, _>>()?;
      let term = ctx.new_term_ref();
      term.unify(&*terms)?;
      Ok(term)
    },
  }
}

/// Generate a query from a list of terms. Returns the arguments to be given to
/// `rewrite/2`.
///
/// # Errors
///
/// This function will error if any term could not be transformed into a
/// queryable format.
pub fn generate_query<'a>(ctx:&'a Context<'a, ActivatedEngine<'a>>, terms:&'a [AstTerm])
                          -> PrologResult<(Term<'a>, Term<'a>)> {
  let terms =
    terms.iter().rev().map(|term| transform_term(ctx, term)).collect::<Result<Vec<_>, _>>()?;
  let var = ctx.new_term_ref();

  Ok((term!(ctx: #&*terms)?, var))
}

fn generate_single<'a>(ctx:&'a Context<'a, ActivatedEngine<'a>>, rule:&'a Rule)
                       -> PrologResult<Clause<'a>> {
  let rest = ctx.new_term_ref();

  let replace =
    rule.rep.iter().rev().map(|term| transform_term(ctx, term)).collect::<Result<Vec<_>, _>>()?;
  let replace_term = ctx.new_term_ref();
  ctx.unify_list(&replace_term, &*replace, &rest)?;

  let pattern =
    rule.pat.iter().rev().map(|term| transform_term(ctx, term)).collect::<Result<Vec<_>, _>>()?;
  let pattern_term = ctx.new_term_ref();
  ctx.unify_list(&pattern_term, &*pattern, &rest)?;

  let other = ctx.new_term_ref();
  let body = term!(ctx: equiv(#&replace_term, #&other))?;
  let clause = ctx.create_clause("user_equiv", 2)?;
  clause.vars.get(0).unwrap().unify(term!(ctx: #&pattern_term)?)?;
  clause.vars.get(1).unwrap().unify(term!(ctx: #&other)?)?;
  clause.body.unify(body)?;

  Ok(clause)
}

/// Generate a list of [`Clause`]s from a list of rules.
///
/// # Errors
///
/// This function will error if any rule could not be compiled.
pub fn generate<'a>(ctx:&'a Context<'a, ActivatedEngine<'a>>, rules:&'a [Rule])
                    -> PrologResult<Vec<Clause<'a>>> {
  rules.iter().map(|rule| generate_single(ctx, rule)).collect()
}
