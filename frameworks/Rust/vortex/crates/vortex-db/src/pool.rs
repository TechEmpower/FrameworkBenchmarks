//! Per-thread PostgreSQL connection pool.
//!
//! Round-robin selection with no synchronization needed (thread-local counter).
//! Connections are pre-established and statements pre-prepared at startup.

use crate::connection::{DbConfig, PgConnection};
use std::io;
use std::net::SocketAddr;

/// A per-thread pool of PostgreSQL connections.
pub struct PgPool {
    connections: Vec<PgConnection>,
    next: usize,
}

impl PgPool {
    /// Create a pool with `n` connections using a pre-resolved address.
    pub fn new_resolved(addr: SocketAddr, config: &DbConfig, n: usize) -> io::Result<Self> {
        let mut connections = Vec::with_capacity(n);
        for _ in 0..n {
            connections.push(PgConnection::connect_resolved(addr, config)?);
        }
        Ok(Self {
            connections,
            next: 0,
        })
    }

    /// Create a pool with `n` connections (resolves DNS).
    pub fn new(config: &DbConfig, n: usize) -> io::Result<Self> {
        let addr = PgConnection::resolve_host(config)?;
        Self::new_resolved(addr, config, n)
    }

    /// Get the next connection (round-robin).
    #[inline]
    pub fn get(&mut self) -> &mut PgConnection {
        let idx = self.next;
        self.next = (self.next + 1) % self.connections.len();
        &mut self.connections[idx]
    }

    /// Number of connections in the pool.
    pub fn len(&self) -> usize {
        self.connections.len()
    }
}
