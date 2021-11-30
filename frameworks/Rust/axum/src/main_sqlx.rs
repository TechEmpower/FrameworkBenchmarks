mod models_common;
mod models_sqlx;
mod database_sqlx;
mod utils;
mod server;
mod common;

use dotenv::dotenv;
use std::env;
use crate::database_sqlx::{DatabaseConnection};
use axum::{
    extract::{Query},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    AddExtensionLayer, Json, Router,
};
use axum::http::{header, HeaderValue};
use tower_http::set_header::SetResponseHeaderLayer;
use hyper::Body;
use rand::rngs::SmallRng;
use rand::{SeedableRng};
use sqlx::PgPool;
use yarte::Template;

use models_sqlx::{World, Fortune};
use database_sqlx::create_pool;
use utils::{Params, parse_params, random_number, Utf8Html};

async fn db(DatabaseConnection(mut conn): DatabaseConnection) -> impl IntoResponse {
    let mut rng = SmallRng::from_entropy();
    let number = random_number(&mut rng);

    let world : World = sqlx::query_as("SELECT id, randomnumber FROM World WHERE id = $1").bind(number)
        .fetch_one(&mut conn).await.ok().expect("error loading world");

    (StatusCode::OK, Json(world))
}

async fn queries(DatabaseConnection(mut conn): DatabaseConnection, Query(params): Query<Params>) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();

    let mut results = Vec::with_capacity(q);

    for _ in 0..q {
        let query_id = random_number(&mut rng);

        let result :World =  sqlx::query_as("SELECT * FROM World WHERE id = $1").bind(query_id)
            .fetch_one(&mut conn).await.ok().expect("error loading world");

        results.push(result);
    }

    (StatusCode::OK, Json(results))
}

async fn fortunes(DatabaseConnection(mut conn): DatabaseConnection) -> impl IntoResponse {
    let mut fortunes: Vec<Fortune> = sqlx::query_as("SELECT * FROM Fortune").fetch_all(&mut conn).await
        .ok().expect("Could not load Fortunes");

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

async fn updates(DatabaseConnection(mut conn): DatabaseConnection, Query(params): Query<Params>) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();

    let mut results = Vec::with_capacity(q);

    for _ in 0..q {
        let query_id = random_number(&mut rng);
        let mut result :World =  sqlx::query_as("SELECT * FROM World WHERE id = $1").bind(query_id)
            .fetch_one(&mut conn).await.ok().expect("error loading world");

        result.random_number = random_number(&mut rng);
        results.push(result);
    }

    for w in &results {
        sqlx::query("UPDATE World SET randomnumber = $1 WHERE id = $2")
            .bind(w.random_number).bind(w.id)
            .execute(&mut conn)
            .await.ok().expect("could not update world");
    }

    (StatusCode::OK, Json(results))
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url = env::var("AXUM_TECHEMPOWER_DATABASE_URL").ok()
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
        .route("/queries", get(queries))
        .route("/updates", get(updates))
        .layer(AddExtensionLayer::new(pool))
        .layer(SetResponseHeaderLayer::<_, Body>::if_not_present(header::SERVER, HeaderValue::from_static("Axum")))
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}