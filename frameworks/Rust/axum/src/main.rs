extern crate serde_derive;
extern crate dotenv;
extern crate async_trait;
extern crate tokio_pg_mapper_derive;
extern crate tokio_pg_mapper;

mod models_common;
mod server;
mod common;

use models_common::{Message};

use axum::http::StatusCode;
use axum::Json;
use dotenv::dotenv;
use axum::{Router, routing::get};
use axum::http::{header, HeaderValue};
use axum::response::IntoResponse;
use tower_http::set_header::SetResponseHeaderLayer;
use hyper::Body;

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

    let app =  Router::new()
        .route("/plaintext", get(plaintext))
        .route("/json", get(json))
        .layer(SetResponseHeaderLayer::<_, Body>::if_not_present(header::SERVER, HeaderValue::from_static("Axum")));

    server::builder()
        .http1_pipeline_flush(true)
        .serve(app.into_make_service())
        .await
        .unwrap();
}