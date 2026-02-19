use std::sync::Arc;

use axum::Router;
use chopin_core::FastRoute;
use serde::Serialize;
use tracing::info;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    let config = chopin_core::Config::from_env()?;
    chopin_core::perf::init_date_cache();

    // ── Both routes use FastRoute (no Axum Router needed!) ──
    let fast_routes = Arc::new([
        // Plaintext: zero-alloc static response (~35ns)
        FastRoute::text("/plaintext", b"Hello, World!").get_only(),
        // JSON: per-request serialization with thread-local buffer reuse (~100-150ns)
        FastRoute::json_serialize("/json", || Message {
            message: "Hello, World!",
        })
        .get_only(),
    ]);

    let addr: std::net::SocketAddr = config.server_addr().parse()?;
    info!("Starting Chopin server on {}", addr);

    // Empty Axum Router (no routes needed!)
    let router = Router::new();

    chopin_core::server::run_reuseport(addr, fast_routes, router, std::future::pending())
        .await?;

    Ok(())
}