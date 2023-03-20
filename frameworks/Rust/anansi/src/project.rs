use anansi::project::prelude::*;

#[cfg(feature = "raw")]
use super::hello::middleware::{Pg, Stmt};

#[cfg(feature = "raw")]
use crate::impl_pg;

#[cfg(feature = "raw")]
app_cache!(local);

#[cfg(not(feature = "raw"))]
app_cache!(redis);

database!(postgres);

#[cfg(feature = "raw")]
raw_middleware!();

#[cfg(feature = "raw")]
anansi::setup!(stmt, Stmt, Pg);

#[cfg(feature = "raw")]
impl_pg!();

#[cfg(not(feature = "raw"))]
middleware!();
