//! vortex-io: io_uring reactor layer for Vortex
//!
//! Provides the foundational I/O abstraction built on Linux io_uring
//! with multishot accept/recv, provided buffer rings, and zero-copy send.

#[cfg(feature = "io-uring")]
pub mod uring;

#[cfg(feature = "epoll-fallback")]
pub mod epoll;

pub mod common;

// Re-export the primary backend
#[cfg(feature = "io-uring")]
pub use uring::Ring;
