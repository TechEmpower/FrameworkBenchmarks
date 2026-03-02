//! vortex-server: HTTP server combining all Vortex layers.
//!
//! Provides the Server builder API that wires vortex-io, vortex-runtime,
//! and vortex-http into a working HTTP server.

pub mod server;

pub use server::Server;
