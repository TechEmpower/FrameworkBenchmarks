use axum::extract::{Extension, FromRequest, RequestParts};
use axum::http::StatusCode;
use deadpool_postgres::{Client, Manager, ManagerConfig, RecyclingMethod};
use std::io;
use std::str::FromStr;
use tokio_postgres::NoTls;

use crate::common::MAX_POOL_SIZE;
use crate::utils::internal_error;

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

pub async fn create_pool(database_url: String) -> deadpool_postgres::Pool {
    let pg_config =
        tokio_postgres::Config::from_str(&*database_url).expect("invalid database url");

    let mgr_config = ManagerConfig {
        recycling_method: RecyclingMethod::Fast,
    };
    let mgr = Manager::from_config(pg_config, NoTls, mgr_config);
    let pool: deadpool_postgres::Pool = deadpool_postgres::Pool::builder(mgr)
        .max_size(MAX_POOL_SIZE as usize)
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
