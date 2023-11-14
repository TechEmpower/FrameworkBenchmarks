use std::io;

use axum::{
    async_trait,
    extract::FromRequestParts,
    http::{request::Parts, StatusCode},
};
use sqlx::{
    pool::PoolConnection,
    postgres::{PgArguments, PgPoolOptions},
    Arguments, PgPool, Postgres,
};

use crate::{utils::internal_error, Fortune, World};

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
impl FromRequestParts<PgPool> for DatabaseConnection {
    type Rejection = (StatusCode, String);

    async fn from_request_parts(
        _parts: &mut Parts,
        pool: &PgPool,
    ) -> Result<Self, Self::Rejection> {
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
