pub use swipl::fli;
pub use swipl::prelude::*;

pub mod codegen;
pub mod util;
pub use util::ContextExt;

static SAVED_STATE:&[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/mlatu.pl.save"));

#[must_use]
pub fn init_engine() -> Engine {
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
