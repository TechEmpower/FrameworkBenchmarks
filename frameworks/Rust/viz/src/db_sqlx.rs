use viz::{
    async_trait, Error, FromRequest, IntoResponse, Request, RequestExt, Response,
    StatusCode,
};

use crate::utils::get_query_param;

pub use sqlx::{
    pool::PoolConnection,
    postgres::{PgArguments, PgPool, PgPoolOptions},
    Postgres,
};

pub struct DatabaseConnection(pub PoolConnection<Postgres>);

#[async_trait]
impl FromRequest for DatabaseConnection {
    type Error = PgError;

    async fn extract(req: &mut Request) -> Result<Self, Self::Error> {
        req.state::<PgPool>()
            .ok_or(PgError(sqlx::Error::Io(std::io::Error::from(
                std::io::ErrorKind::NotConnected,
            ))))?
            .acquire()
            .await
            .map(Self)
            .map_err(PgError)
    }
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct PgError(#[from] pub sqlx::Error);

impl From<PgError> for Error {
    fn from(e: PgError) -> Self {
        Error::Responder(e.into_response())
    }
}

impl IntoResponse for PgError {
    fn into_response(self) -> Response {
        (StatusCode::INTERNAL_SERVER_ERROR, self.to_string()).into_response()
    }
}

pub struct Counter(pub u16);

#[async_trait]
impl FromRequest for Counter {
    type Error = Error;

    async fn extract(req: &mut Request) -> Result<Self, Self::Error> {
        Ok(Counter(get_query_param(req.query_string())))
    }
}
