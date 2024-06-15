#[cfg(feature = "jemallocator")]
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

mod application;
mod db;
mod routes;
use application::application;
use trillium::HttpConfig;

#[cfg(all(feature = "smol", not(feature = "tokio"), not(feature = "async-std")))]
use trillium_smol::config;

#[cfg(all(feature = "tokio", not(feature = "smol"), not(feature = "async-std")))]
use trillium_tokio::config;

#[cfg(all(feature = "async-std", not(feature = "smol"), not(feature = "tokio")))]
use trillium_async_std::config;

#[cfg(not(any(feature = "async-std", feature = "smol", feature = "tokio")))]
compile_error! {"please run with one of the following --features `async-std`, `smol`, `tokio`"}

fn main() {
    #[cfg(debug_assertions)]
    env_logger::init();

    let http_config = HttpConfig::default()
        .with_response_buffer_len(256)
        .with_request_buffer_initial_len(256)
        .with_response_header_initial_capacity(5);

    config()
        .with_nodelay()
        .with_http_config(http_config)
        .run(application())
}
