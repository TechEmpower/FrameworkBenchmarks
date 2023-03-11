use axum::{
    http::{header, HeaderValue, StatusCode},
    response::IntoResponse,
    routing::get,
    Json, Router,
};
use dotenv::dotenv;
use rand::{rngs::SmallRng, thread_rng, Rng, SeedableRng};
use sqlx::PgPool;
use tower_http::set_header::SetResponseHeaderLayer;
use yarte::Template;

mod database_sqlx;
mod models_common;
mod models_sqlx;
mod server;
mod utils;

use self::{
    database_sqlx::{create_pool, fetch_fortunes, fetch_world, DatabaseConnection},
    models_sqlx::{Fortune, World},
    utils::get_environment_variable,
    utils::Utf8Html,
};

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

async fn db(DatabaseConnection(conn): DatabaseConnection) -> impl IntoResponse {
    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();

    let random_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

    let world = fetch_world(conn, random_id)
        .await
        .expect("could not fetch world");

    (StatusCode::OK, Json(world))
}

async fn fortunes(DatabaseConnection(conn): DatabaseConnection) -> impl IntoResponse {
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

    let database_url: String = get_environment_variable("AXUM_TECHEMPOWER_DATABASE_URL");
    let max_pool_size: u32 = get_environment_variable("AXUM_TECHEMPOWER_MAX_POOL_SIZE");
    let min_pool_size: u32 = get_environment_variable("AXUM_TECHEMPOWER_MIN_POOL_SIZE");

    // setup connection pool
    let pool = create_pool(database_url, max_pool_size, min_pool_size).await;

    let app = router(pool).await;

    server::builder()
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn router(pool: PgPool) -> Router {
    let server_header_value = HeaderValue::from_static("Axum");

    Router::new()
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .with_state(pool)
        .layer(SetResponseHeaderLayer::if_not_present(
            header::SERVER,
            server_header_value,
        ))
}
