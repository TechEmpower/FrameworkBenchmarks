use saphir::prelude::*;

mod json;
mod plain;

pub static HELLO_WORLD: &'static str = "Hello, world!";

#[tokio::main]
async fn main() -> Result<(), SaphirError> {
    let server = Server::builder()
        .configure_listener(|l| {
            l.interface("0.0.0.0:8080")
        })
        .configure_router(|r| {
            r
                .controller(json::JsonController)
                .controller(plain::PlainController)
        })
        .build();

    server.run().await
}