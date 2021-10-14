pub use swipl::fli;
pub use swipl::prelude::*;

use crate::ast::Term as AstTerm;

pub mod codegen;
pub mod util;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
pub use util::ContextExt;

static SAVED_STATE:&[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/mlatu.pl.save"));

type PrologContext<'a> = Context<'a, ActivatedEngine<'a>>;

/// Initialize the prolog engine.
///
/// # Panics
///
/// This function will panic if the engine has already been initialized.
#[must_use]
pub fn init_engine() -> Engine {
  // To maintain safety, SWIPL must not have been initialized yet.
  assert!(!is_swipl_initialized(),
          "SWIPL must not be initialized when getting an engine with a saved state");
  let bytes = SAVED_STATE.as_ptr();
  // SAFETY: This is called *before* `PL_initialise` (called when creating an
  // engine). This data is also valid and the length is correct, as we take the
  // pointer and length from a known valid slice.
  let len = u64::try_from(SAVED_STATE.len()).expect("length");
  unsafe {
    fli::PL_set_resource_db_mem(bytes, len);
  }

  Engine::new()
}

pub fn thread(start:impl for<'a> FnOnce(&'a PrologContext<'a>, &'a Module),
              tx:&UnboundedSender<Result<Vec<AstTerm>, String>>,
              mut rx:UnboundedReceiver<Vec<AstTerm>>) {
  let engine = init_engine();
  let ctx:PrologContext<'_> = engine.activate().into();

  let module = Module::new("mlatu");

  start(&ctx, &module);

  while let Some(terms) = rx.blocking_recv() {
    if terms.is_empty() {
      tx.send(Ok(vec![])).expect("send result");
    }

    let (list, other) = match codegen::generate_query(&ctx, &*terms) {
      | Ok(x) => x,
      | Err(err) => {
        tx.send(Err(format!("Error while compiling query: {}", err))).expect("send result");
        continue
      },
    };

    if let Err(err) = ctx.call_once(pred![mlatu: rewrite / 2], [&list, &other]) {
      tx.send(Err(format!("Error while executing query: {}", err))).expect("send result");
      continue
    }

    tx.send(match other.get::<Vec<AstTerm>>() {
        | Ok(terms) => Ok(terms.into_iter().rev().collect()),
        | Err(err) => Err(format!("Error while getting result: {}", err)),
      })
      .expect("send result");
  }
}
