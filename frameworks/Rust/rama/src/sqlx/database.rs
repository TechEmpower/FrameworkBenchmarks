use std::io;

use sqlx::{postgres::PgPoolOptions, PgPool};

#[derive(Debug)]
#[allow(dead_code)]
pub enum PgError {
    Io(io::Error),
    Pg(sqlx::Error),
}

impl From<io::Error> for PgError {
    fn from(err: io::Error) -> Self {
        PgError::Io(err)
    }
}

impl From<sqlx::Error> for PgError {
    fn from(err: sqlx::Error) -> Self {
        PgError::Pg(err)
    }
}

pub async fn create_pool(
    database_url: String,
    max_pool_size: u32,
    min_pool_size: u32,
) -> PgPool {
    PgPoolOptions::new()
        .max_connections(max_pool_size)
        .min_connections(min_pool_size)
        .test_before_acquire(false)
        .connect(&database_url)
        .await
        .unwrap()
}
