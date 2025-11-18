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
    config
        .host("0.0.0.0")
        .await
        .port(8080)
        .await
        .disable_linger()
        .await
        .disable_nodelay()
        .await
        .buffer(256)
        .await;
    Server::from(config)
        .await
        .request_middleware(middleware::request)
        .await
        .route("/plaintext", route::plaintext)
        .await
        .route("/json", route::json)
        .await
        .route("/cached-quer", route::cached_query)
        .await
        .route("/db", route::db)
        .await
        .route("/query", route::query)
        .await
        .route("/fortunes", route::fortunes)
        .await
        .route("/upda", route::update)
        .await
        .run()
        .await
        .unwrap()
        .wait()
        .await;
}

async fn init() {
    init_db().await;
    init_server().await;
}

pub fn run_server() {
    runtime().block_on(init());
}
