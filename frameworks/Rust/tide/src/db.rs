//! Database related utilities.

use diesel::{pg::PgConnection, r2d2};
use std::time::Duration;

const DATABASE_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

/// A shared pool of DB connections used across the server.
pub type Pool = r2d2::Pool<r2d2::ConnectionManager<PgConnection>>;

/// A single connection, checked out from the shared pool.
pub type Connection = r2d2::PooledConnection<r2d2::ConnectionManager<PgConnection>>;

/// Setup the shared database connection pool, configured for benchmarking.
pub fn connect() -> Pool {
    let manager = r2d2::ConnectionManager::new(DATABASE_URL);
    r2d2::Builder::new()
        // Double maxiumum benchmark concurrency
        .max_size(1024)
        // Minimum benchmark concurrency
        .min_idle(Some(16))
        .connection_timeout(Duration::from_secs(5))
        .build(manager)
        .expect("building r2d2 connection pool")
}
