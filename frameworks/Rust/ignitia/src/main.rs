mod common;

use common::models::Message;
use ignitia::{Response, Router, Server};

const HELLO_WORLD: &str = "Hello, World!";

#[inline(always)]
async fn plaintext() -> Response {
    Response::text(HELLO_WORLD)
        .with_header("Server", "Ignitia")
        .with_header("Content-Type", "text/plain")
        .with_header(
            "Date",
            httpdate::fmt_http_date(std::time::SystemTime::now()),
        )
}

#[inline(always)]
async fn json() -> Response {
    // Create new Message object for each request (no caching)
    let message = Message {
        message: HELLO_WORLD,
    };

    Response::json(message)
        .with_header("Server", "Ignitia")
        .with_header("Content-Type", "application/json")
        .with_header(
            "Date",
            httpdate::fmt_http_date(std::time::SystemTime::now()),
        )
}

#[tokio::main]
async fn main() {
    dotenv::dotenv().ok();

    let app = Router::new()
        .get("/plaintext", plaintext)
        .get("/json", json);

    println!("Starting Ignitia server on 0.0.0.0:8000");

    Server::new(app, "0.0.0.0:8000".parse().unwrap())
        .with_performance_config(ignitia::PerformanceConfig::max_rps())
        .ignitia()
        .await
        .unwrap();
}
