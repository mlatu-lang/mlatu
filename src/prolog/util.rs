use std::os::raw::{c_char, c_int};

use swipl::fli;
use swipl::prelude::*;

/// A clause to be asserted.
#[derive(Clone, Debug)]
pub struct Clause<'a> {
  pub head:Term<'a>,
  pub body:Term<'a>,
  /// The actual clause that can be asserted.
  pub clause:Term<'a>,
  /// A vector of variables that are unified with the variables in the clause's
  /// head.
  pub vars:Vec<Term<'a>>,
}

pub trait ContextExt<'a> {
  /// Create a [`Clause`] with the given context, name, and arity.
  /// This creates a body to be unified and a clause that can be asserted once
  /// the body is defined.
  ///
  /// # Errors
  ///
  /// This function errors if any terms could not be unified.
  fn create_clause(&'a self, name:impl IntoAtom, arity:u16) -> PrologResult<Clause<'a>>;
  /// Asserts a clause.
  ///
  /// # Errors
  ///
  /// This function errors if the term could not be asserted.
  fn assert(&'a self, term:&Term<'a>, module:Option<&'a Module>, location:AssertLocation)
            -> PrologResult<()>;
  /// Write a term to a string using `write_canonical`.
  fn canonical(&'a self, term:&Term<'a>) -> Option<String>;
  /// Unify a term with the elements and the tail of a list.
  ///
  /// # Errors
  ///
  /// This function errors if any terms could not be unified.
  fn unify_list(&'a self, term:&Term<'a>, elems:&'a [Term<'a>], tail:&Term<'a>)
                -> PrologResult<()>;
}

/// Location at which a clause is asserted.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AssertLocation {
  /// Clause is to be inserted first (asserta).
  First,
  /// Clause is to be inserted last (assertz).
  Last,
}

impl<'a> ContextExt<'a> for Context<'a, ActivatedEngine<'a>> {
  fn create_clause(&'a self, name:impl IntoAtom, arity:u16) -> PrologResult<Clause<'a>> {
    let head = self.new_term_ref();
    let body = self.new_term_ref();
    let clause = self.new_term_ref();
    head.unify(Functor::new(name, arity))?;
    let vars = (1..=arity).map(|n| {
                            let term = self.new_term_ref();
                            head.unify_arg(usize::from(n), &term).map(|_| term)
                          })
                          .collect::<Result<Vec<_>, _>>()?;
    clause.unify(Functor::new(":-", 2))?;
    clause.unify_arg(1, &head)?;
    clause.unify_arg(2, &body)?;

    Ok(Clause { head, body, clause, vars })
  }

  fn assert(&'a self, term:&Term<'a>, module:Option<&'a Module>, location:AssertLocation)
            -> PrologResult<()> {
    let module = module.map_or_else(std::ptr::null_mut, Module::module_ptr);

    let flags = match location {
      | AssertLocation::First => c_int::try_from(fli::PL_ASSERTA).expect("converting PL_ASSERTA \
                                                                          to a c_int \
                                                                          unexpectedly wrapped"),
      | AssertLocation::Last => c_int::try_from(fli::PL_ASSERTZ).expect("converting PL_ASSERTZ \
                                                                         to a c_int unexpectedly \
                                                                         wrapped"),
    };

    // SAFETY: The term and module are valid, and a null module is fine.
    // (PL_assert calls assert_term in src/pl-comp.c, which is called in other
    // places in pl-comp.c with NULL for the module parameter). Our flags are valid
    // as well.
    let res = unsafe { fli::PL_assert(term.term_ptr(), module, flags) };
    if res == c_int::try_from(fli::TRUE).expect("converting TRUE to a c_int unexpectedly wrapped") {
      Ok(())
    } else if res
              == c_int::try_from(fli::FALSE).expect("converting FALSE to a c_int unexpectdly \
                                                     wrapped")
    {
      Err(PrologError::Exception)
    } else {
      Err(PrologError::Failure)
    }
  }

  fn canonical(&'a self, term:&Term<'a>) -> Option<String> {
    let mut len:usize = 0;
    let len_ptr:*mut usize = &mut len;
    let size_t_ptr:*mut fli::size_t = len_ptr.cast();
    let mut s:*mut c_char = std::ptr::null_mut();
    let flags = fli::CVT_WRITE_CANONICAL | fli::BUF_DISCARDABLE | fli::REP_UTF8;
    // SAFETY: Our term is valid, our pointers are valid, and our flags are valid.
    let result = unsafe { fli::PL_get_nchars(term.term_ptr(), size_t_ptr, &mut s, flags) };
    if result == 0 {
      None
    } else {
      // SAFETY: The pointer we got from PL_get_nchars is valid for at least `len`.
      let slice = unsafe { std::slice::from_raw_parts(s.cast::<u8>(), len) };

      Some(String::from_utf8(Vec::from(slice)).expect("utf-8"))
    }
  }

  fn unify_list(&'a self, term:&Term<'a>, elems:&'a [Term<'a>], tail:&Term<'a>)
                -> PrologResult<()> {
    let list = self.new_term_ref();
    list.unify(&term)?;
    let mut success = true;

    for t in elems.iter() {
      // create a new frame to ensure we don't just keep putting head and tail refs on
      // the stack.
      let frame = self.open_frame();
      let head = frame.new_term_ref();
      let tail = frame.new_term_ref();
      // SAFETY: List, head, and tail are valid term refs.
      success =
        unsafe { fli::PL_unify_list(list.term_ptr(), head.term_ptr(), tail.term_ptr()) != 0 };

      if !success {
        break
      }

      head.unify(t)?;

      // reset term - should really be a method on term
      // SAFETY: List is a valid term ref
      unsafe { fli::PL_put_variable(list.term_ptr()) };

      list.unify(tail)?;

      frame.close();
    }

    if success {
      list.unify(&tail)?;
      Ok(())
    } else {
      Err(PrologError::Failure)
    }
  }
}
