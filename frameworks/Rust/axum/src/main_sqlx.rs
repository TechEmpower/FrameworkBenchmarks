extern crate dotenv;
extern crate serde_derive;
#[macro_use]
extern crate async_trait;

mod common;
mod database_sqlx;
mod models_common;
mod models_sqlx;
mod server;
mod utils;

use crate::database_sqlx::{
    fetch_fortunes, fetch_world, update_world, DatabaseConnection,
};
use axum::http::{header, HeaderValue};
use axum::{
    extract::{Extension, Query},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    Json, Router,
};
use dotenv::dotenv;
use futures_util::stream::FuturesUnordered;
use futures_util::TryStreamExt;
use rand::rngs::SmallRng;
use rand::{thread_rng, Rng, SeedableRng};
use sqlx::{PgPool, Postgres};
use std::borrow::BorrowMut;
use std::env;
use tower_http::set_header::SetResponseHeaderLayer;
use yarte::Template;

use database_sqlx::create_pool;
use models_sqlx::{Fortune, World};
use utils::{parse_params, random_number, Params, Utf8Html};

async fn db(DatabaseConnection(mut conn): DatabaseConnection) -> impl IntoResponse {
    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();

    let random_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

    let world = fetch_world(conn, random_id)
        .await
        .expect("could not fetch world");

    (StatusCode::OK, Json(world))
}

async fn fortunes(
    DatabaseConnection(mut conn): DatabaseConnection,
) -> impl IntoResponse {
    let mut fortunes = fetch_fortunes(conn)
        .await
        .expect("could not fetch fortunes");

    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    Utf8Html(
        FortunesTemplate {
            fortunes: &fortunes,
        }
        .call()
        .expect("error rendering template"),
    )
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url = env::var("AXUM_TECHEMPOWER_DATABASE_URL")
        .ok()
        .expect("AXUM_TECHEMPOWER_DATABASE_URL environment variable was not set");

    // setup connection pool
    let pool = create_pool(database_url).await;

    let app = router(pool).await;

    server::builder()
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn router(pool: PgPool) -> Router {
    Router::new()
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .layer(Extension(pool))
        .layer(SetResponseHeaderLayer::if_not_present(
            header::SERVER,
            HeaderValue::from_static("Axum"),
        ))
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}
