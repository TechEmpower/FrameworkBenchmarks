use std::sync::Arc;

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

    // chopin-core 0.3.5+: FastRoute computes headers (Content-Length, Date, Server) per-request.
    // TFB compliance is maintained regardless of route type.
    let fast_routes = Arc::new([
        // Plaintext: static body, per-request headers
        FastRoute::text("/plaintext", b"Hello, World!").get_only(),
        // JSON: per-request serialization and per-request headers
        FastRoute::json_serialize("/json", || Message {
            message: "Hello, World!",
        })
        .get_only(),
    ]);

    let addr: std::net::SocketAddr = config.server_addr().parse()?;
    info!("Starting Chopin server on {}", addr);

    // Empty Router (all routes handled by FastRoute)
    let router = chopin_core::Router::new();

    chopin_core::server::run_reuseport(addr, fast_routes, router, std::future::pending())
        .await?;

    Ok(())
}