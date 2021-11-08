use axum::extract::{Extension, FromRequest, RequestParts};
use axum::http::StatusCode;
use bb8::{Pool, PooledConnection};
use bb8_postgres::PostgresConnectionManager;
use bb8_postgres::tokio_postgres::NoTls;
use crate::utils::internal_error;

pub type ConnectionManager = PostgresConnectionManager<NoTls>;
pub type ConnectionPool = Pool<ConnectionManager>;
pub type Connection = PooledConnection<'static, ConnectionManager>;

pub async fn create_bb8_pool(database_url: String) -> ConnectionPool {
    let manager = PostgresConnectionManager::new_from_stringlike(database_url, NoTls).unwrap();

    Pool::builder().build(manager).await.unwrap()
}

pub struct DatabaseConnection(pub Connection);

#[async_trait]
impl<B> FromRequest<B> for DatabaseConnection
    where
        B: Send,
{
    type Rejection = (StatusCode, String);

    async fn from_request(req: &mut RequestParts<B>) -> Result<Self, Self::Rejection> {
        let Extension(pool) = Extension::<ConnectionPool>::from_request(req)
            .await
            .map_err(internal_error)?;

        let conn = pool.get_owned().await.map_err(internal_error)?;

        Ok(Self(conn))
    }
}