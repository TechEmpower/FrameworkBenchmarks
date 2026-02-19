use std::sync::Arc;

use chopin_core::Router;
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

    
    let fast_routes = Arc::new([
        // TFB plaintext test: static body is allowed to be pre-built and reused.
        // FastRoute::text() stores a pre-built Bytes and clones the pointer (not the data)
        // on every request — zero allocation, zero serialization needed.
        FastRoute::text("/plaintext", b"Hello, World!").get_only(),

        // TFB JSON test: TFB requires that serialization must NOT be cached;
        // the computational work must happen within each request.
        //
        // FastRoute::json_serialize() does this:
        //   1. Calls the closure `|| Message { ... }` — once per request.
        //   2. Serializes the returned struct to JSON — once per request.
        //   3. Writes into a thread-local BytesMut buffer — reused for allocation
        //      efficiency only; the serialization itself is never skipped or cached.
        //
        // This is different from FastRoute::json("/json", br#"{"message":"..."}"#),
        // which pre-builds the bytes at startup and serves the same Bytes every time
        // (no per-request serialization — TFB non-compliant for the JSON test).
        FastRoute::json_serialize("/json", || Message {
            message: "Hello, World!",
        })
        .get_only(),
    ]);

    let addr: std::net::SocketAddr = config.server_addr().parse()?;
    info!("Starting Chopin server on {}", addr);

    // Empty Chopin Router (no routes needed!)
    let router = Router::new();

    chopin_core::server::run_reuseport(addr, fast_routes, router, std::future::pending())
        .await?;

    Ok(())
}