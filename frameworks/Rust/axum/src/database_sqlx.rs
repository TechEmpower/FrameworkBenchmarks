use axum::extract::{Extension, FromRequest, RequestParts};
use axum::http::StatusCode;

use async_trait::async_trait;
use sqlx::{PgPool, Postgres};
use sqlx::pool::PoolConnection;
use sqlx::postgres::PgPoolOptions;
use crate::common::POOL_SIZE;
use crate::utils::internal_error;

pub async fn create_pool(database_url: String) -> PgPool {
    PgPoolOptions::new().max_connections(POOL_SIZE).min_connections(56).connect(&*database_url).await.unwrap()
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

