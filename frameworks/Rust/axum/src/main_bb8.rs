extern crate serde_derive;
extern crate dotenv;
#[macro_use]
extern crate async_trait;

mod common_handlers;
mod models_common;
mod models_bb8;
mod database_bb8;
mod utils;

use dotenv::dotenv;
use std::net::{Ipv4Addr, SocketAddr};
use std::env;
use crate::database_bb8::{Connection, create_bb8_pool, DatabaseConnection};
use axum::{
    extract::{Query},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    AddExtensionLayer, Json, Router,
};
use axum::http::{header, HeaderValue};
use bb8_postgres::tokio_postgres::{Row, Statement};
use tower_http::set_header::SetResponseHeaderLayer;
use hyper::Body;
use rand::rngs::SmallRng;
use rand::{SeedableRng};
use tokio_pg_mapper::FromTokioPostgresRow;
use yarte::Template;

use models_bb8::{World, Fortune};
use common_handlers::{json, plaintext};
use utils::{Params, parse_params, random_number};
use crate::utils::Utf8Html;

async fn db(DatabaseConnection(conn): DatabaseConnection) -> impl IntoResponse {
    let mut rng = SmallRng::from_entropy();
    let number = random_number(&mut rng);

    let select = prepare_fetch_world_by_id_statement(&conn).await;
    let world = fetch_world_by_id_using_statement(&conn, number, &select).await;

    (StatusCode::OK, Json(world))
}

async fn fetch_world_by_id_using_statement(conn: &Connection, number: i32, select: &Statement) -> World {
    let row: Row = conn.query_one(select, &[&number]).await.unwrap();

    World::from_row(row).unwrap()
}

async fn queries(DatabaseConnection(conn): DatabaseConnection, Query(params): Query<Params>) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();

    let mut results = Vec::with_capacity(q as usize);

    let select = prepare_fetch_world_by_id_statement(&conn).await;

    for _ in 0..q {
        let query_id = random_number(&mut rng);

        let result :World = fetch_world_by_id_using_statement(&conn, query_id, &select).await;

        results.push(result);
    }

    (StatusCode::OK, Json(results))
}

async fn fortunes(DatabaseConnection(conn): DatabaseConnection) -> impl IntoResponse {
    let select = prepare_fetch_all_fortunes_statement(&conn).await;

    let rows: Vec<Row> = conn.query(&select, &[]).await.unwrap();

    let mut fortunes: Vec<Fortune> = Vec::with_capacity(rows.capacity());;

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

async fn updates(DatabaseConnection(conn): DatabaseConnection, Query(params): Query<Params>) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();

    let mut results = Vec::with_capacity(q as usize);

    let select = prepare_fetch_world_by_id_statement(&conn).await;

    for _ in 0..q {
        let query_id = random_number(&mut rng);
        let mut result :World = fetch_world_by_id_using_statement(&conn, query_id, &select).await;

        result.randomnumber = random_number(&mut rng);
        results.push(result);
    }

    let update = prepare_update_world_by_id_statement(&conn).await;

    for w in &results {
        conn.execute(&update, &[&w.randomnumber, &w.id]).await.unwrap();
    }

    (StatusCode::OK, Json(results))
}

async fn prepare_fetch_all_fortunes_statement(conn: &Connection) -> Statement {
    conn.prepare("SELECT * FROM Fortune").await.unwrap()
}

async fn prepare_fetch_world_by_id_statement(conn: &Connection) -> Statement {
    conn.prepare("SELECT id, randomnumber FROM World WHERE id = $1").await.unwrap()
}

async fn prepare_update_world_by_id_statement(conn: &Connection) -> Statement {
    conn.prepare("UPDATE World SET randomnumber = $1 WHERE id = $2").await.unwrap()
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url = env::var("AXUM_TECHEMPOWER_DATABASE_URL").ok()
        .expect("AXUM_TECHEMPOWER_DATABASE_URL environment variable was not set");

    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8000));

    // setup connection pool
    let pool = create_bb8_pool(database_url).await;

    let router = Router::new()
        .route("/plaintext", get(plaintext))
        .route("/json", get(json))
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .route("/queries", get(queries))
        .route("/updates", get(updates))
        .layer(AddExtensionLayer::new(pool))
        .layer(SetResponseHeaderLayer::<_, Body>::if_not_present(header::SERVER, HeaderValue::from_static("Axum")));

    axum::Server::bind(&addr)
        .serve(router.into_make_service())
        .await
        .unwrap();
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}
