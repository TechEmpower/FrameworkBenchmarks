mod common;
mod database_bb8;
mod models_bb8;
mod models_common;
mod server;
mod utils;

use crate::database_bb8::{create_bb8_pool, Connection, DatabaseConnection};
use axum::http::{header, HeaderValue};
use axum::{
    extract::Query, http::StatusCode, response::IntoResponse, routing::get, Extension,
    Json, Router,
};
use bb8_postgres::tokio_postgres::{Row, Statement};
use dotenv::dotenv;
use rand::rngs::SmallRng;
use rand::SeedableRng;
use std::env;
use tokio_pg_mapper::FromTokioPostgresRow;
use tower_http::set_header::SetResponseHeaderLayer;
use yarte::Template;

use crate::utils::Utf8Html;
use models_bb8::{Fortune, World};
use utils::{parse_params, random_number, Params};

async fn db(DatabaseConnection(conn): DatabaseConnection) -> impl IntoResponse {
    let mut rng = SmallRng::from_entropy();
    let number = random_number(&mut rng);

    let select = prepare_fetch_world_by_id_statement(&conn).await;
    let world = fetch_world_by_id_using_statement(&conn, number, &select).await;

    (StatusCode::OK, Json(world))
}

async fn fetch_world_by_id_using_statement(
    conn: &Connection,
    number: i32,
    select: &Statement,
) -> World {
    let row: Row = conn.query_one(select, &[&number]).await.unwrap();

    World::from_row(row).unwrap()
}

async fn queries(
    DatabaseConnection(conn): DatabaseConnection,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();

    let mut results = Vec::with_capacity(q);

    let select = prepare_fetch_world_by_id_statement(&conn).await;

    for _ in 0..q {
        let query_id = random_number(&mut rng);

        let result: World =
            fetch_world_by_id_using_statement(&conn, query_id, &select).await;

        results.push(result);
    }

    (StatusCode::OK, Json(results))
}

async fn fortunes(DatabaseConnection(conn): DatabaseConnection) -> impl IntoResponse {
    let select = prepare_fetch_all_fortunes_statement(&conn).await;

    let rows: Vec<Row> = conn.query(&select, &[]).await.unwrap();

    let mut fortunes: Vec<Fortune> = Vec::with_capacity(rows.capacity());

    for row in rows {
        fortunes.push(Fortune::from_row(row).unwrap());
    }

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

async fn updates(
    DatabaseConnection(conn): DatabaseConnection,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();

    let mut results = Vec::with_capacity(q);

    let select = prepare_fetch_world_by_id_statement(&conn).await;

    for _ in 0..q {
        let query_id = random_number(&mut rng);
        let mut result: World =
            fetch_world_by_id_using_statement(&conn, query_id, &select).await;

        result.randomnumber = random_number(&mut rng);
        results.push(result);
    }

    let update = prepare_update_world_by_id_statement(&conn).await;

    for w in &results {
        conn.execute(&update, &[&w.randomnumber, &w.id])
            .await
            .unwrap();
    }

    (StatusCode::OK, Json(results))
}

async fn prepare_fetch_all_fortunes_statement(conn: &Connection) -> Statement {
    conn.prepare("SELECT * FROM Fortune").await.unwrap()
}

async fn prepare_fetch_world_by_id_statement(conn: &Connection) -> Statement {
    conn.prepare("SELECT id, randomnumber FROM World WHERE id = $1")
        .await
        .unwrap()
}

async fn prepare_update_world_by_id_statement(conn: &Connection) -> Statement {
    conn.prepare("UPDATE World SET randomnumber = $1 WHERE id = $2")
        .await
        .unwrap()
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url = env::var("AXUM_TECHEMPOWER_DATABASE_URL")
        .expect("AXUM_TECHEMPOWER_DATABASE_URL environment variable was not set");

    // setup connection pool
    let pool = create_bb8_pool(database_url).await;

    let router = Router::new()
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .route("/queries", get(queries))
        .route("/updates", get(updates))
        .layer(Extension(pool))
        .layer(SetResponseHeaderLayer::if_not_present(
            header::SERVER,
            HeaderValue::from_static("Axum"),
        ));

    server::builder()
        .serve(router.into_make_service())
        .await
        .unwrap();
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}
