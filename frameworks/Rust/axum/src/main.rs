extern crate serde_derive;
extern crate dotenv;
#[macro_use]
extern crate async_trait;

mod models;
mod database_sqlx;
mod common;

use dotenv::dotenv;
use std::net::{Ipv4Addr, SocketAddr};
use axum::{Router, routing::get};
use axum::http::{header, HeaderValue};
use tower_http::set_header::SetResponseHeaderLayer;
use hyper::Body;

use crate::common::{json, plaintext};

#[tokio::main]
async fn main() {
    dotenv().ok();

    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8000));

    let app = router().await;

    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn router() -> Router {
    Router::new()
        .route("/plaintext", get(plaintext))
        .route("/json", get(json))
        .layer(SetResponseHeaderLayer::<_, Body>::if_not_present(header::SERVER, HeaderValue::from_static("Axum")))
}

