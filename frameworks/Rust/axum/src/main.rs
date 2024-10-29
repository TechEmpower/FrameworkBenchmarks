mod common;
mod server;

use axum::{http::StatusCode, response::IntoResponse, routing::get, Router};
use common::models::Message;
use dotenv::dotenv;

#[cfg(not(feature = "simd-json"))]
use axum::Json;
#[cfg(feature = "simd-json")]
use common::simd_json::Json;

/// Return a plaintext static string.
#[inline(always)]
pub async fn plaintext() -> &'static str {
    "Hello, World!"
}

/// Return a JSON message.
#[inline(always)]
pub async fn json() -> impl IntoResponse {
    let message = Message {
        message: "Hello, World!",
    };

    (StatusCode::OK, Json(message))
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let app = Router::new()
        .route("/plaintext", get(plaintext))
        .route("/json", get(json));

    server::serve_hyper(app, Some(8000)).await
}
