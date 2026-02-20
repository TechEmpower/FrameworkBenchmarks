use super::*;

pub(crate) fn init_server_config() -> ServerConfig {
    let mut server_config: ServerConfig = ServerConfig::default();
    server_config
        .set_address(Server::format_bind_address(DEFAULT_HOST, 8080))
        .set_nodelay(Some(false));
    server_config
}

pub(crate) fn init_request_config() -> RequestConfig {
    RequestConfig::low_security()
}
