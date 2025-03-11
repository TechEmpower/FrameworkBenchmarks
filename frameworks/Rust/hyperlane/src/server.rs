use crate::*;

pub async fn run_server() {
    let mut server: Server = Server::new();
    server.host("0.0.0.0").await;
    server.port(60000).await;
    server.log_dir("./logs").await;
    server.log_interval_millis(1_000_000_000).await;
    server.disable_print().await;
    server.router("/json", json).await;
    server.router("/plaintext", plaintext).await;
    server.router("/db", db).await;
    server.router("/queries", queries).await;
    server.request_middleware(request).await;
    server.response_middleware(response).await;
    println_success!("server initialization completed");
    server.listen().await;
}
