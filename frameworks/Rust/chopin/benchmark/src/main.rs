use std::sync::Arc;

use chopin_core::extract::Json;
use chopin_core::http::header;
use chopin_core::http::HeaderValue;
use chopin_core::response::IntoResponse;
use chopin_core::routing::get;
use chopin_core::FastRoute;
use chopin_core::Router;
use serde::Serialize;
use tracing::info;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

// ── Handlers ────────────────────────────────────────────────────────────────

/// Plaintext: static body is allowed by TFB.
/// Headers (Content-Length, Date, Server) are computed per-request by chopin-core 0.3.5+.
async fn plaintext_handler() -> impl IntoResponse {
    (
        [(header::SERVER, HeaderValue::from_static("chopin"))],
        "Hello, World!",
    )
}

/// JSON: per-request serialization required by TFB.
/// chopin-core 0.3.5+ computes all headers (Content-Length, Date, Server) per-request.
async fn json_handler() -> impl IntoResponse {
    (
        [(header::SERVER, HeaderValue::from_static("chopin"))],
        Json(Message {
            message: "Hello, World!",
        }),
    )
}

// ── Main ────────────────────────────────────────────────────────────────────

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    let config = chopin_core::Config::from_env()?;
    chopin_core::perf::init_date_cache();

    // TFB compliance: chopin-core 0.3.5+ computes headers per-request, not cached.
    let router = Router::new()
        .route("/plaintext", get(plaintext_handler))
        .route("/json", get(json_handler));

    let addr: std::net::SocketAddr = config.server_addr().parse()?;
    info!("Starting Chopin server on {}", addr);

    // Empty FastRoute list (only use Router for TFB compliance)
    let fast_routes: Arc<[FastRoute]> = Arc::new([]);

    chopin_core::server::run_reuseport(addr, fast_routes, router, std::future::pending())
        .await?;

    Ok(())
}