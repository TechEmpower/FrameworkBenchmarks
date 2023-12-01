mod application;
mod db;
mod routes;
use application::application;
use trillium::HttpConfig;

fn main() {
    #[cfg(debug_assertions)]
    env_logger::init();

    let http_config = HttpConfig::default()
        .with_response_buffer_len(256)
        .with_request_buffer_initial_len(256)
        .with_response_header_initial_capacity(5);

    trillium_smol::config()
        .with_http_config(http_config)
        .run(application())
}
