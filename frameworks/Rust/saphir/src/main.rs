use std::time::Duration;
use saphir::prelude::*;
use mongodm::prelude::*;

mod controller;
mod errors;
mod models;
mod templates;
mod cache;

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<(), SaphirError> {
    // let mut client_options = MongoClientOptions::parse("mongodb://localhost:27017")
    let mut client_options = MongoClientOptions::parse("mongodb://tfb-database:27017")
        .await
        .map_err(|e| SaphirError::Custom(Box::new(e)))?;

    client_options.min_pool_size = Some(64);
    client_options.max_pool_size = Some(512);

    client_options.connect_timeout = Some(Duration::from_millis(200));
    let client = MongoClient::with_options(client_options).map_err(|e| SaphirError::Custom(Box::new(e)))?;
    let db = client.database("hello_world");

    let server = Server::builder()
        .configure_listener(|l| {
            l.interface("0.0.0.0:8080")
        })
        .configure_router(|r| {
            r
                .controller(controller::BenchmarkController::new(db))
        })
        .build();

    server.run().await
}