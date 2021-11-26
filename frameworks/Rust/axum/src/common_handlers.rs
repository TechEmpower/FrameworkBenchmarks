use axum::http::StatusCode;
use axum::Json;
use axum::response::IntoResponse;

use crate::models_common::{Message};

pub async fn plaintext() -> &'static str {
    "Hello, World!"
}

pub async fn json() -> impl IntoResponse {
    let message = Message {
        message: "Hello, World!",
    };

    (StatusCode::OK, Json(message))
}