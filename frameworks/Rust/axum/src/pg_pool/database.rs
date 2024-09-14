use std::io;

use crate::{
    common::utils::internal_error,
    pg_pool::models::{Fortune, World},
};
use axum::{
    async_trait,
    extract::FromRequestParts,
    http::{request::Parts, StatusCode},
};
use deadpool_postgres::{Client, Manager, ManagerConfig, RecyclingMethod};
use futures_util::StreamExt;
use tokio::pin;
use tokio_pg_mapper::FromTokioPostgresRow;
use tokio_postgres::{NoTls, Row, Statement};

#[derive(Debug)]
#[allow(dead_code)]
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
    let pg_config: tokio_postgres::Config =
        database_url.parse().expect("invalid database url");

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
impl FromRequestParts<deadpool_postgres::Pool> for DatabaseClient {
    type Rejection = (StatusCode, String);

    async fn from_request_parts(
        _parts: &mut Parts,
        pool: &deadpool_postgres::Pool,
    ) -> Result<Self, Self::Rejection> {
        let conn = pool.get().await.map_err(internal_error)?;

        Ok(Self(conn))
    }
}

pub async fn fetch_world_by_id(
    client: &Client,
    id: i32,
    select: &Statement,
) -> Result<World, PgError> {
    let row: Row = client.query_one(select, &[&id]).await.unwrap();

    Ok(World::from_row(row).unwrap())
}

pub async fn fetch_all_fortunes(
    client: Client,
    select: &Statement,
) -> Result<Vec<Fortune>, PgError> {
    let mut fortunes: Vec<Fortune> = Vec::new();
    let rows = client.query_raw::<_, _, &[i32; 0]>(select, &[]).await?;
    pin!(rows);

    while let Some(row) = rows.next().await.transpose()? {
        fortunes
            .push(Fortune::from_row(row).expect("could not convert row to fortune."));
    }

    Ok(fortunes)
}
