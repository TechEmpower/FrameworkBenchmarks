use axum::async_trait;
use axum::extract::{Extension, FromRequest, RequestParts};
use axum::http::StatusCode;
use deadpool_postgres::{Client, Manager, ManagerConfig, RecyclingMethod};
use std::io;
use std::str::FromStr;
use tokio_pg_mapper::FromTokioPostgresRow;
use tokio_postgres::{NoTls, Row, Statement};

use crate::utils::internal_error;
use crate::{Fortune, World};

#[derive(Debug)]
pub enum PgError {
    Io(io::Error),
    Pg(tokio_postgres::Error),
}

impl From<io::Error> for PgError {
    fn from(err: io::Error) -> Self {
        PgError::Io(err)
    }
}

impl From<tokio_postgres::Error> for PgError {
    fn from(err: tokio_postgres::Error) -> Self {
        PgError::Pg(err)
    }
}

pub async fn create_pool(
    database_url: String,
    max_pool_size: u32,
) -> deadpool_postgres::Pool {
    let pg_config =
        tokio_postgres::Config::from_str(&*database_url).expect("invalid database url");

    let mgr_config = ManagerConfig {
        recycling_method: RecyclingMethod::Fast,
    };
    let mgr = Manager::from_config(pg_config, NoTls, mgr_config);
    let pool: deadpool_postgres::Pool = deadpool_postgres::Pool::builder(mgr)
        .max_size(max_pool_size as usize)
        .build()
        .unwrap();

    pool
}

pub struct DatabaseClient(pub Client);

#[async_trait]
impl<B> FromRequest<B> for DatabaseClient
where
    B: Send,
{
    type Rejection = (StatusCode, String);

    async fn from_request(req: &mut RequestParts<B>) -> Result<Self, Self::Rejection> {
        let Extension(pool) = Extension::<deadpool_postgres::Pool>::from_request(req)
            .await
            .map_err(internal_error)?;

        let conn = pool.get().await.map_err(internal_error)?;

        Ok(Self(conn))
    }
}

pub async fn fetch_world_by_id(
    client: &Client,
    number: i32,
    select: &Statement,
) -> Result<World, PgError> {
    let row: Row = client.query_one(select, &[&number]).await.unwrap();

    Ok(World::from_row(row).unwrap())
}

pub async fn update_world(
    client: &Client,
    update: &Statement,
    random_id: i32,
    w_id: i32,
) -> Result<u64, PgError> {
    let rows_modified: u64 = client.execute(update, &[&random_id, &w_id]).await.unwrap();

    Ok(rows_modified)
}

pub async fn fetch_all_fortunes(
    client: Client,
    select: &Statement,
) -> Result<Vec<Fortune>, PgError> {
    let rows: Vec<Row> = client.query(select, &[]).await.unwrap();

    let mut fortunes: Vec<Fortune> = Vec::with_capacity(rows.capacity());

    for row in rows {
        fortunes.push(Fortune::from_row(row).unwrap());
    }

    Ok(fortunes)
}

pub async fn prepare_fetch_all_fortunes_statement(client: &Client) -> Statement {
    client
        .prepare_cached("SELECT * FROM Fortune")
        .await
        .unwrap()
}

pub async fn prepare_fetch_world_by_id_statement(client: &Client) -> Statement {
    client
        .prepare_cached("SELECT id, randomnumber FROM World WHERE id = $1")
        .await
        .unwrap()
}

pub async fn prepare_update_world_by_id_statement(client: &Client) -> Statement {
    client
        .prepare_cached("UPDATE World SET randomnumber = $1 WHERE id = $2")
        .await
        .unwrap()
}
