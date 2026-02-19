use std::sync::Arc;

use axum::http::HeaderValue;
use axum::response::IntoResponse;
use axum::routing::get;
use axum::{http::header, Json, Router};
use chopin_core::FastRoute;
use serde::Serialize;

// ── Models ──────────────────────────────────────────────────────────────────

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

// ── Handlers ────────────────────────────────────────────────────────────────

/// JSON Serialization test (TFB requirement: per-request serialization)
async fn json_handler() -> impl IntoResponse {
    let msg = Message {
        message: "Hello, World!",
    };
    (
        [(header::SERVER, HeaderValue::from_static("chopin"))],
        Json(msg),
    )
}

// ── Main ────────────────────────────────────────────────────────────────────

#[tokio::main(flavor = "current_thread")]
async fn main() {
    // ── Chopin Config: reads SERVER_HOST, SERVER_PORT, REUSEPORT ──
    let config = chopin_core::Config::from_env().expect("failed to load Chopin config");

    // ── Chopin Perf: start lock-free Date header cache (AtomicU64 + thread-local) ──
    chopin_core::perf::init_date_cache();

    // ── FastRoute for plaintext (TFB allows caching for plaintext, not JSON) ──
    let fast_routes: Arc<[FastRoute]> = Arc::from(vec![
        FastRoute::text("/plaintext", b"Hello, World!").get_only(),
    ]);

    // ── Axum Router for /json (requires per-request serialization per TFB spec) ──
    let router = Router::new().route("/json", get(json_handler));

    // ── Chopin Server: SO_REUSEPORT multi-core accept loops ──
    let addr: std::net::SocketAddr = config.server_addr().parse().unwrap();
    chopin_core::server::run_reuseport(addr, fast_routes, router, std::future::pending())
        .await
        .expect("server error");
}
