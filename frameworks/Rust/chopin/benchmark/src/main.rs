use std::sync::Arc;

use axum::Router;
use chopin_core::FastRoute;

// ── Main ────────────────────────────────────────────────────────────────────

#[tokio::main(flavor = "current_thread")]
async fn main() {
    // ── Chopin Config: reads SERVER_HOST, SERVER_PORT, REUSEPORT ──
    let config = chopin_core::Config::from_env().expect("failed to load Chopin config");

    // ── Chopin Perf: start lock-free Date header cache (AtomicU64 + thread-local) ──
    chopin_core::perf::init_date_cache();

    // ── Static routes via Chopin FastRoute (zero-allocation, ~35ns/req) ──
    // Benchmark endpoints — /json, /plaintext
    let fast_routes: Arc<[FastRoute]> = Arc::from(vec![
        FastRoute::json("/json", br#"{"message":"Hello, World!"}"#).get_only(),
        FastRoute::text("/plaintext", b"Hello, World!").get_only(),
    ]);

    // ── Empty Axum Router (no DB endpoints needed) ──
    let router: Router = Router::new();

    // ── Chopin Server: SO_REUSEPORT multi-core accept loops ──
    let addr: std::net::SocketAddr = config.server_addr().parse().unwrap();
    chopin_core::server::run_reuseport(addr, fast_routes, router, std::future::pending())
        .await
        .expect("server error");
}
