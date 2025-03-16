use crate::*;

#[inline]
pub async fn run_server() {
    let mut server: Server = Server::new();
    server.host("0.0.0.0").await;
    server.port(8080).await;
    server.log_dir("./logs").await;
    server.log_interval_millis(1_000_000_000).await;
    // server.disable_print().await;
    server.route("/j", json).await;
    server.route("/p", plaintext).await;
    server.route("/d", db).await;
    server.route("/q", queries).await;
    server.route("/f", fortunes).await;
    server.route("/u", updates).await;
    server.route("/c", cached_queries).await;
    server.request_middleware(request).await;
    server.response_middleware(response).await;
    server.listen().await;
}
