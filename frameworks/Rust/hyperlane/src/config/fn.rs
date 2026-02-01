use super::*;

pub(crate) async fn init_server_config() -> ServerConfig {
    let server_config: ServerConfig = ServerConfig::new().await;
    server_config.port(8080).await;
    server_config.disable_nodelay().await;
    server_config
}

pub(crate) async fn init_request_config() -> RequestConfig {
    let request_config: RequestConfig = RequestConfig::new().await;
    request_config
        .buffer_size(KB_4)
        .await
        .http_read_timeout_ms(u64::MAX)
        .await
        .max_body_size(usize::MAX)
        .await
        .max_header_count(usize::MAX)
        .await
        .max_header_key_length(usize::MAX)
        .await
        .max_header_line_length(usize::MAX)
        .await
        .max_header_value_length(usize::MAX)
        .await
        .max_path_length(usize::MAX)
        .await
        .max_query_length(usize::MAX)
        .await
        .max_request_line_length(usize::MAX)
        .await
        .max_ws_frame_size(usize::MAX)
        .await
        .max_ws_frames(usize::MAX)
        .await
        .ws_read_timeout_ms(u64::MAX)
        .await;
    request_config
}
