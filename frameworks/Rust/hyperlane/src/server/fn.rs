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
    let config: ServerConfig = ServerConfig::new().await;
    config.host("0.0.0.0").await;
    config.port(8080).await;
    config.disable_nodelay().await;
    config.http_buffer(256).await;
    config.ws_buffer(256).await;

    let server: Server = Server::from(config).await;

    server.request_middleware(middleware::request).await;

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

    server.run().await.unwrap().wait().await;
}

async fn init() {
    init_db().await;
    init_server().await;
}

pub fn run_server() {
    runtime().block_on(init());
}
