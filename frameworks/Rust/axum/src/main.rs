use axum::{
    http::{header, HeaderValue, StatusCode},
    response::IntoResponse,
    routing::get,
    Json, Router,
};
use dotenv::dotenv;
use tower_http::set_header::SetResponseHeaderLayer;

mod models_common;
mod server;

use self::models_common::Message;

pub async fn plaintext() -> &'static str {
    "Hello, World!"
}

pub async fn json() -> impl IntoResponse {
    let message = Message {
        message: "Hello, World!",
    };

    (StatusCode::OK, Json(message))
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let server_header_value = HeaderValue::from_static("Axum");

    let app = Router::new()
        .route("/plaintext", get(plaintext))
        .route("/json", get(json))
        .layer(SetResponseHeaderLayer::if_not_present(
            header::SERVER,
            server_header_value,
        ));

    server::builder()
        .http1_pipeline_flush(true)
        .serve(app.into_make_service())
        .await
        .unwrap();
}
