extern crate dotenv;
extern crate serde_derive;
#[macro_use]
extern crate async_trait;

mod common;
mod database_pg_pool;
mod models_common;
mod models_pg_pool;
mod server;
mod utils;

use crate::database_pg_pool::{create_pool, DatabaseClient, PgError};
use axum::http::{header, HeaderValue};
use axum::{
    extract::Query, http::StatusCode, response::IntoResponse, routing::get, Extension,
    Json, Router,
};
use deadpool_postgres::Client;
use dotenv::dotenv;
use futures_util::stream::FuturesUnordered;
use futures_util::TryStreamExt;
use rand::rngs::SmallRng;
use rand::{thread_rng, Rng, SeedableRng};
use std::env;
use tokio_pg_mapper::FromTokioPostgresRow;
use tokio_postgres::{Row, Statement};
use tower_http::set_header::SetResponseHeaderLayer;
use yarte::Template;

use crate::utils::Utf8Html;
use models_pg_pool::{Fortune, World};
use utils::{parse_params, random_number, Params};

async fn db(DatabaseClient(client): DatabaseClient) -> impl IntoResponse {
    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();

    let random_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

    let select = prepare_fetch_world_by_id_statement(&client).await;
    let world = fetch_world_by_id_using_statement(&client, random_id, &select)
        .await
        .expect("could not fetch world");

    (StatusCode::OK, Json(world))
}

async fn fetch_world_by_id_using_statement(
    client: &Client,
    number: i32,
    select: &Statement,
) -> Result<World, PgError> {
    let row: Row = client.query_one(select, &[&number]).await.unwrap();

    Ok(World::from_row(row).unwrap())
}

async fn queries(
    DatabaseClient(client): DatabaseClient,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();

    let select = prepare_fetch_world_by_id_statement(&client).await;

    let future_worlds = FuturesUnordered::new();

    for _ in 0..q {
        let w_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

        future_worlds.push(fetch_world_by_id_using_statement(&client, w_id, &select));
    }

    let worlds: Result<Vec<World>, PgError> = future_worlds.try_collect().await;
    let results = worlds.expect("worlds could not be retrieved");

    (StatusCode::OK, Json(results))
}

async fn fortunes(DatabaseClient(client): DatabaseClient) -> impl IntoResponse {
    let select = prepare_fetch_all_fortunes_statement(&client).await;

    let rows: Vec<Row> = client.query(&select, &[]).await.unwrap();

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
    DatabaseClient(client): DatabaseClient,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();

    let select = prepare_fetch_world_by_id_statement(&client).await;

    let future_worlds = FuturesUnordered::new();

    for _ in 0..q {
        let query_id = random_number(&mut rng);

        future_worlds.push(fetch_world_by_id_using_statement(
            &client, query_id, &select,
        ));
    }

    let worlds: Result<Vec<World>, PgError> = future_worlds.try_collect().await;
    let results = worlds.expect("worlds could not be retrieved");

    let update = prepare_update_world_by_id_statement(&client).await;

    let future_world_updates = FuturesUnordered::new();

    //let mut transaction = client.transaction().await.expect("expected to start a transaction");
    for w in &results {
        let random_id = random_number(&mut rng);
        let w_id = w.id;

        future_world_updates.push(update_world_using_statement(
            &client, &update, random_id, w_id,
        ));
    }
    //transaction.commit().await;

    let world_updates: Result<Vec<u64>, PgError> =
        future_world_updates.try_collect().await;
    world_updates.expect("updates could not be executed");

    (StatusCode::OK, Json(results))
}

async fn update_world_using_statement(
    client: &Client,
    update: &Statement,
    random_id: i32,
    w_id: i32,
) -> Result<u64, PgError> {
    let rows_modified: u64 = client.execute(update, &[&random_id, &w_id]).await.unwrap();

    Ok(rows_modified)
}

async fn prepare_fetch_all_fortunes_statement(client: &Client) -> Statement {
    client
        .prepare_cached("SELECT * FROM Fortune")
        .await
        .unwrap()
}

async fn prepare_fetch_world_by_id_statement(client: &Client) -> Statement {
    client
        .prepare_cached("SELECT id, randomnumber FROM World WHERE id = $1")
        .await
        .unwrap()
}

async fn prepare_update_world_by_id_statement(client: &Client) -> Statement {
    client
        .prepare_cached("UPDATE World SET randomnumber = $1 WHERE id = $2")
        .await
        .unwrap()
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url = env::var("AXUM_TECHEMPOWER_DATABASE_URL")
        .ok()
        .expect("AXUM_TECHEMPOWER_DATABASE_URL environment variable was not set");

    // setup Client pool
    let pool = create_pool(database_url).await;

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
