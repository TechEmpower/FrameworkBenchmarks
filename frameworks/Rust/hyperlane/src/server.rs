use super::*;

fn runtime() -> Runtime {
    Builder::new_multi_thread()
        .worker_threads(get_thread_count())
        .thread_stack_size(1_048_576)
        .max_blocking_threads(2_048)
        .max_io_events_per_tick(1_024)
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
    server.http_buffer(256).await;
    server.ws_buffer(256).await;

    server.request_middleware(request_middleware::request).await;

    server
        .disable_http_hook("/plaintext")
        .await
        .route("/plaintext", route::plaintext)
        .await;

    server
        .disable_http_hook("/json")
        .await
        .route("/json", route::json)
        .await;

    server
        .disable_http_hook("/cached-quer")
        .await
        .route("/cached-quer", route::cached_query)
        .await;

    server
        .disable_http_hook("/db")
        .await
        .route("/db", route::db)
        .await;

    server
        .disable_http_hook("/query")
        .await
        .route("/query", route::query)
        .await;

    server
        .disable_http_hook("/fortunes")
        .await
        .route("/fortunes", route::fortunes)
        .await;

    server
        .disable_http_hook("/upda")
        .await
        .route("/upda", route::update)
        .await;

    server.run().await.unwrap().get_wait_hook()().await;
}

async fn init() {
    #[cfg(any(feature = "dev"))]
    init_db().await;
    init_server().await;
}

pub fn run_server() {
    runtime().block_on(init());
}
