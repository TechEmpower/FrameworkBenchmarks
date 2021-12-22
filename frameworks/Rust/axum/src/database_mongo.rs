use axum::extract::{Extension, FromRequest, RequestParts};
use axum::http::StatusCode;

use mongodb::{Client, Database};
use crate::utils::internal_error;

pub struct DatabaseConnection(pub Database);

#[async_trait]
impl<B> FromRequest<B> for DatabaseConnection
    where
        B: Send,
{
    type Rejection = (StatusCode, String);

    async fn from_request(req: &mut RequestParts<B>) -> Result<Self, Self::Rejection> {
        let Extension(client) = Extension::<Client>::from_request(req)
            .await
            .map_err(internal_error)?;

        let database = client.database("hello_world");

        Ok(Self(database))
    }
}

