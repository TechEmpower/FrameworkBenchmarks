use crate::*;

#[inline]
pub async fn run_server() {
    let mut server: Server = Server::new();
    server.host("0.0.0.0").await;
    server.port(8080).await;
    server.log_dir("./logs").await;
    server.log_interval_millis(1_000_000_000).await;
    // server.disable_print().await;
    server.route("/json", json).await;
    server.route("/plaintext", plaintext).await;
    server.route("/db", db).await;
    server.route("/queries", queries).await;
    server.route("/fortunes", fortunes).await;
    server.route("/updates", updates).await;
    server.request_middleware(request).await;
    server.response_middleware(response).await;
    server.listen().await;
}
