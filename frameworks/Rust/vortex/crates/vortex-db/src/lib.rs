//! vortex-db: Custom PostgreSQL wire protocol client for Vortex.
//!
//! Implements the minimum PostgreSQL protocol needed for TechEmpower:
//! - Startup + authentication (trust, cleartext, MD5, SCRAM-SHA-256)
//! - Prepared statements (Parse/Bind/Execute/Sync)
//! - Binary format for parameters and results
//! - Pipelined queries for /queries and /updates
//! - Per-thread connection pooling

pub mod wire;
pub mod scram;
pub mod connection;
pub mod pool;

pub use connection::{DbConfig, PgConnection};
pub use pool::PgPool;

/// Thread-local RNG — seeded once per thread, not per call.
use nanorand::{Rng, WyRand};
use std::cell::RefCell;

thread_local! {
    static RNG: RefCell<WyRand> = RefCell::new(WyRand::new());
}

/// Generate a random World ID (1..=10000).
#[inline]
pub fn random_world_id() -> i32 {
    RNG.with(|rng| (rng.borrow_mut().generate_range(0_u32..10000) + 1) as i32)
}

/// Clamp a queries parameter to [1, 500].
#[inline]
pub fn clamp_queries(n: i32) -> i32 {
    if n < 1 { 1 } else if n > 500 { 500 } else { n }
}
