use anansi::project::prelude::*;

#[cfg(feature = "raw")]
app_cache!(local);

#[cfg(not(feature = "raw"))]
app_cache!(redis);

database!(postgres);

middleware!();
