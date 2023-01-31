use axum::async_trait;
use axum::extract::{Extension, FromRequestParts};
use axum::http::request::Parts;
use axum::http::StatusCode;
use std::io;

use crate::utils::internal_error;
use crate::{Fortune, World};
use sqlx::pool::PoolConnection;
use sqlx::postgres::{PgArguments, PgPoolOptions};
use sqlx::{Arguments, PgPool, Postgres};

#[derive(Debug)]
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
        .connect(&database_url)
        .await
        .unwrap()
}

pub struct DatabaseConnection(pub PoolConnection<Postgres>);

#[async_trait]
impl<S> FromRequestParts<S> for DatabaseConnection
where
    S: Send + Sync,
{
    type Rejection = (StatusCode, String);
    async fn from_request_parts(
        parts: &mut Parts,
        state: &S,
    ) -> Result<Self, Self::Rejection> {
        let Extension(pool) = Extension::<PgPool>::from_request_parts(parts, state)
            .await
            .map_err(internal_error)?;

        let conn = pool.acquire().await.map_err(internal_error)?;

        Ok(Self(conn))
    }
}

pub async fn fetch_world(
    mut conn: PoolConnection<Postgres>,
    number: i32,
) -> Result<World, PgError> {
    let mut args = PgArguments::default();
    args.add(number);

    let world: World =
        sqlx::query_as_with("SELECT id, randomnumber FROM World WHERE id = $1", args)
            .fetch_one(&mut conn)
            .await
            .expect("error loading world");
    Ok(world)
}

pub async fn fetch_fortunes(
    mut conn: PoolConnection<Postgres>,
) -> Result<Vec<Fortune>, PgError> {
    let fortunes: Vec<Fortune> = sqlx::query_as("SELECT * FROM Fortune")
        .fetch_all(&mut conn)
        .await
        .expect("error loading Fortunes");
    Ok(fortunes)
}
