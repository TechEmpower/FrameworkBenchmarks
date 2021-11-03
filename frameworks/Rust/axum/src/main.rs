extern crate serde_derive;
extern crate dotenv;
#[macro_use]
extern crate async_trait;

mod models;
mod database;

use std::convert::Infallible;
use dotenv::dotenv;
use std::net::{Ipv4Addr, SocketAddr};
use std::env;
use yarte::Template;
use crate::database::{DatabaseConnection};
use axum::{
    extract::{Query},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    AddExtensionLayer, Json, Router,
};
use axum::body::{Bytes, Full};
use axum::http::{header, HeaderValue, Response};
use serde::{Deserialize};
use tower_http::set_header::SetResponseHeaderLayer;
use hyper::Body;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use sqlx::PgPool;

use models::{World, Fortune, Message};
use database::create_pool;

#[derive(Debug, Deserialize)]
struct Params {
    queries: Option<String>,
}

async fn plaintext() -> &'static str {
    "Hello, World!"
}

async fn json() -> impl IntoResponse {
    let message = Message {
        message: "Hello, World!",
    };

    (StatusCode::OK, Json(message))
}

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

    let mut results = Vec::with_capacity(q as usize);

    for _ in 0..q {
        let query_id = random_number(&mut rng);

        let result :World =  sqlx::query_as("SELECT * FROM World WHERE id = $1").bind(query_id)
            .fetch_one(&mut conn).await.ok().expect("error loading world");

        results.push(result);
    }

    (StatusCode::OK, Json(results))
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
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

    let mut results = Vec::with_capacity(q as usize);

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

fn random_number(rng: &mut SmallRng) -> i32 {
    (rng.gen::<u32>() % 10_000 + 1) as i32
}

fn parse_params(params: Params) -> i32 {
    let mut q = 0;

    if params.queries.is_some() {
        let queries = params.queries.ok_or("could not get value").unwrap();

        let queries_as_int = queries.parse::<i32>();

        match queries_as_int {
            Ok(_ok) => q = queries_as_int.unwrap(),
            Err(_e) => q = 1,
        }
    }

    let q = if q == 0 {
        1
    } else if q > 500 {
        500
    } else {
        q
    };

    q
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url = env::var("AXUM_TECHEMPOWER_DATABASE_URL").ok()
        .expect("AXUM_TECHEMPOWER_DATABASE_URL environment variable was not set");

    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8000));

    // setup connection pool
    let pool = create_pool(database_url).await;

    let app = router(pool).await;

    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn router(pool: PgPool) -> Router {
    Router::new()
        .route("/plaintext", get(plaintext))
        .route("/json", get(json))
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .route("/queries", get(queries))
        .route("/updates", get(updates))
        .layer(AddExtensionLayer::new(pool))
        .layer(SetResponseHeaderLayer::<_, Body>::if_not_present(header::SERVER, HeaderValue::from_static("Axum")))
}

#[derive(Clone, Copy, Debug)]
pub struct Utf8Html<T>(pub T);

impl<T> IntoResponse for Utf8Html<T>
    where
        T: Into<Full<Bytes>>,
{
    type Body = Full<Bytes>;
    type BodyError = Infallible;

    fn into_response(self) -> Response<Self::Body> {
        let mut res = Response::new(self.0.into());
        res.headers_mut()
            .insert(header::CONTENT_TYPE, HeaderValue::from_static("text/html; charset=utf-8"));
        res
    }
}

impl<T> From<T> for Utf8Html<T> {
    fn from(inner: T) -> Self {
        Self(inner)
    }
}
