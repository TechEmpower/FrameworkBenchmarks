//! vortex-http: High-performance HTTP/1.1 protocol layer for Vortex.
//!
//! Provides tiered HTTP parsing (fast-path for benchmarks, httparse+SIMD
//! for general use) and pre-computed response generation.

pub mod parser;
pub mod request;
pub mod response;
pub mod date;
pub mod pipeline;
