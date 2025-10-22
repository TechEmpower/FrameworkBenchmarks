#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use application::{build_application_state, run};
use server::server_builder;

mod server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    run(server_builder(), build_application_state().await).await?;
    Ok(())
}
