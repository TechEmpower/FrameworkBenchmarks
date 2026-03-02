//! vortex-runtime: Thread-per-core async runtime for Vortex.
//!
//! Each CPU core runs a single worker thread with its own io_uring ring,
//! task executor, and connection state. No shared mutable state between workers.
//! Tasks are !Send — they never move between threads.

pub mod executor;
pub mod worker;
pub mod waker;
pub mod spawn;

pub use worker::Worker;
