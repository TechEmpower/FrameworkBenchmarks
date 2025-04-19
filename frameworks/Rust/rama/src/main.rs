mod common;
mod server;

use common::models::Message;
use dotenv::dotenv;
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[cfg(feature = "simd-json")]
use common::simd_json::Json;
#[cfg(not(feature = "simd-json"))]
use rama::http::response::Json;
use rama::http::{IntoResponse, StatusCode, service::web::Router};

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

fn main() {
    dotenv().ok();
    server::start_tokio(serve_app)
}

async fn serve_app() {
    let app = Router::new()
        .get("/plaintext", plaintext)
        .get("/json", json);

    server::serve((), app, Some(8000)).await;
}
