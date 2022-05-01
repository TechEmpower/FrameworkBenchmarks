use axum::extract::{Extension, FromRequest, RequestParts};
use axum::http::StatusCode;
use std::io;

use crate::common::{MAX_POOL_SIZE, MIN_POOL_SIZE};
use crate::utils::internal_error;
use crate::{Fortune, World};
use sqlx::pool::PoolConnection;
use sqlx::postgres::{PgArguments, PgPoolOptions};
use sqlx::{Arguments, PgConnection, PgPool, Postgres};

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

pub async fn create_pool(database_url: String) -> PgPool {
    PgPoolOptions::new()
        .max_connections(MAX_POOL_SIZE)
        .min_connections(MIN_POOL_SIZE)
        .connect(&*database_url)
        .await
        .unwrap()
}

pub struct DatabaseConnection(pub PoolConnection<Postgres>);

#[async_trait]
impl<B> FromRequest<B> for DatabaseConnection
where
    B: Send,
{
    type Rejection = (StatusCode, String);

    async fn from_request(req: &mut RequestParts<B>) -> Result<Self, Self::Rejection> {
        let Extension(pool) = Extension::<PgPool>::from_request(req)
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
            .ok()
            .expect("error loading world");
    Ok(world)
}

pub async fn fetch_fortunes(
    mut conn: PoolConnection<Postgres>,
) -> Result<Vec<Fortune>, PgError> {
    let mut fortunes: Vec<Fortune> = sqlx::query_as("SELECT * FROM Fortune")
        .fetch_all(&mut conn)
        .await
        .ok()
        .expect("error loading Fortunes");
    Ok(fortunes)
}

pub async fn update_world(
    mut conn: PoolConnection<Postgres>,
    w: &World,
    random_id: i32,
) {
    sqlx::query("UPDATE World SET randomnumber = $1 WHERE id = $2")
        .bind(random_id)
        .bind(w.id)
        .execute(&mut conn)
        .await
        .ok()
        .expect("error updating world");
}
