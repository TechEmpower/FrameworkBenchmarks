use super::*;

pub(crate) async fn init_server_config() -> ServerConfig {
    let server_config: ServerConfig = ServerConfig::new().await;
    server_config.port(8080).await;
    server_config.disable_nodelay().await;
    server_config
}

pub(crate) async fn init_request_config() -> RequestConfig {
    RequestConfig::low_security().await
}
