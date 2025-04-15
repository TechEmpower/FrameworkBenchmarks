use crate::*;
use tokio::runtime::{Builder, Runtime};

fn runtime() -> Runtime {
    Builder::new_multi_thread()
        .worker_threads(get_thread_count() << 1)
        .thread_stack_size(1_048_576)
        .max_blocking_threads(5120)
        .max_io_events_per_tick(5120)
        .global_queue_interval(64)
        .event_interval(1)
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
    server.request_middleware(request).await;
    server.route("/plaintext", plaintext).await;
    server.route("/json", json).await;
    server.route("/cached-quer", cached_queries).await;
    server.route("/db", db).await;
    server.route("/query", queries).await;
    server.route("/fortunes", fortunes).await;
    server.route("/upda", updates).await;
    server.listen().await.unwrap();
}

async fn init() {
    init_db().await;
    init_server().await;
}

pub fn run_server() {
    runtime().block_on(init());
}
