use crate::*;
use tokio::runtime::{Builder, Runtime};

fn runtime() -> Runtime {
    Builder::new_multi_thread()
        .worker_threads(get_thread_count() >> 1)
        .thread_stack_size(1_048_576)
        .max_blocking_threads(5120)
        .max_io_events_per_tick(5120)
        .enable_all()
        .build()
        .unwrap()
}

async fn init_server() {
    let server: Server = Server::new();
    server.host("0.0.0.0").await;
    server.port(8080).await;
    server.disable_linger().await;
    server.disable_nodelay().await;
    server.disable_log().await;
    server.disable_inner_log().await;
    server.disable_inner_print().await;
    server.http_line_buffer_size(256).await;
    server.websocket_buffer_size(256).await;
    server.request_middleware(request_middleware::request).await;
    #[cfg(any(feature = "dev", feature = "plaintext"))]
    server.route("/plaintext", route::plaintext).await;
    #[cfg(any(feature = "dev", feature = "json"))]
    server.route("/json", route::json).await;
    #[cfg(any(feature = "dev", feature = "cached_query"))]
    server.route("/cached-quer", route::cached_query).await;
    #[cfg(any(feature = "dev", feature = "db"))]
    server.route("/db", route::db).await;
    #[cfg(any(feature = "dev", feature = "query"))]
    server.route("/query", route::query).await;
    #[cfg(any(feature = "dev", feature = "fortunes"))]
    server.route("/fortunes", route::fortunes).await;
    #[cfg(any(feature = "dev", feature = "update"))]
    server.route("/upda", route::update).await;
    server.listen().await.unwrap();
}

async fn init() {
    #[cfg(any(
        feature = "dev",
        feature = "db",
        feature = "query",
        feature = "update",
        feature = "fortunes",
        feature = "cached_query",
    ))]
    init_db().await;
    init_server().await;
}

pub fn run_server() {
    runtime().block_on(init());
}
