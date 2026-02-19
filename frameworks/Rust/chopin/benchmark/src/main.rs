use std::sync::Arc;

use axum::response::IntoResponse;
use axum::routing::get;
use axum::{http::HeaderValue, Json, Router};
use axum::http::header;
use chopin_core::FastRoute;
use serde::Serialize;
use tracing::info;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

// ── Handlers ────────────────────────────────────────────────────────────────

/// Plaintext: TFB requires headers to be computed per-request, not cached.
/// Using Axum handler ensures Content-Length and Date are fresh for each request.
async fn plaintext_handler() -> impl IntoResponse {
    (
        [(header::SERVER, HeaderValue::from_static("chopin"))],
        "Hello, World!",
    )
}

/// JSON: TFB requires both serialization and headers to be computed per-request.
/// Using Axum's Json<T> with per-request struct creation.
async fn json_handler() -> impl IntoResponse {
    let msg = Message {
        message: "Hello, World!",
    };
    (
        [(header::SERVER, HeaderValue::from_static("chopin"))],
        Json(msg),
    )
}

// ── Main ────────────────────────────────────────────────────────────────

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    let config = chopin_core::Config::from_env()?;
    chopin_core::perf::init_date_cache();

    // TFB compliance: All headers (Content-Length, Date, Server) must be
    // computed per-request, not cached. FastRoute caches headers, violating
    // the spec. Use Axum Router instead to ensure fresh headers each time.
    let router = Router::new()
        .route("/plaintext", get(plaintext_handler))
        .route("/json", get(json_handler));

    let addr: std::net::SocketAddr = config.server_addr().parse()?;
    info!("Starting Chopin server on {}", addr);

    // Empty FastRoute list (no high-performance cached routes)
    let fast_routes: Arc<[FastRoute]> = Arc::new([]);

    chopin_core::server::run_reuseport(addr, fast_routes, router, std::future::pending())
        .await?;

    Ok(())
}