//! Utilities for random behaviour required by the benchmarks.

use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::cell::RefCell;

thread_local!(
    static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::from_entropy());
);

/// Use a thread local rng with the given closure.
fn with_rng<T>(f: impl FnOnce(&mut SmallRng) -> T) -> T {
    RNG.with(|rng| f(&mut rng.borrow_mut()))
}

/// Return a random number from 1 - 10,000 inclusive.
pub fn random_10k() -> i32 {
    with_rng(|rng| rng.gen_range(1, 10_001))
}
