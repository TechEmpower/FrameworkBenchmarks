mod common;
mod pg_pool;

use axum::{
    extract::Query, http::StatusCode, response::IntoResponse, routing::get, Router,
};

#[cfg(not(feature = "simd-json"))]
use axum::Json;
#[cfg(feature = "simd-json")]
use common::simd_json::Json;

use common::{random_ids, SELECT_ALL_FORTUNES, SELECT_WORLD_BY_ID, UPDATE_WORLDS};
use dotenv::dotenv;
use futures_util::{stream::FuturesUnordered, TryStreamExt};
use rand::{rngs::SmallRng, thread_rng, SeedableRng};
use yarte::Template;

mod server;

use common::{
    get_env, random_id,
    utils::{parse_params, Params, Utf8Html},
};
use pg_pool::database::{
    create_pool, fetch_all_fortunes, fetch_world_by_id, DatabaseClient, PgError,
};
use pg_pool::models::{Fortune, World};

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

async fn db(DatabaseClient(client): DatabaseClient) -> impl IntoResponse {
    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();
    let random_id = random_id(&mut rng);

    let select = &client.prepare_cached(SELECT_WORLD_BY_ID).await.unwrap();
    let world = fetch_world_by_id(&client, random_id, select)
        .await
        .expect("could not fetch world");

    (StatusCode::OK, Json(world))
}

async fn queries(
    DatabaseClient(client): DatabaseClient,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();
    let select = &client.prepare_cached(SELECT_WORLD_BY_ID).await.unwrap();
    let future_worlds = FuturesUnordered::new();

    for id in random_ids(&mut rng, q) {
        future_worlds.push(fetch_world_by_id(&client, id, select));
    }

    let worlds: Result<Vec<World>, PgError> = future_worlds.try_collect().await;
    let results = worlds.expect("worlds could not be retrieved");

    (StatusCode::OK, Json(results))
}

async fn fortunes(DatabaseClient(client): DatabaseClient) -> impl IntoResponse {
    let select = &client.prepare_cached(SELECT_ALL_FORTUNES).await.unwrap();

    let mut fortunes = fetch_all_fortunes(client, select)
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

async fn updates(
    DatabaseClient(client): DatabaseClient,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_entropy();
    let select = &client.prepare_cached(SELECT_WORLD_BY_ID).await.unwrap();
    let update = &client.prepare_cached(UPDATE_WORLDS).await.unwrap();

    // Select the random worlds.
    let future_worlds = FuturesUnordered::new();
    for id in random_ids(&mut rng, q) {
        future_worlds.push(fetch_world_by_id(&client, id, select));
    }
    let worlds: Vec<World> = future_worlds.try_collect().await.unwrap();

    let mut ids = Vec::with_capacity(q);
    let mut nids = Vec::with_capacity(q);
    let worlds: Vec<World> = worlds
        .into_iter()
        .map(|mut w| {
            w.randomnumber = random_id(&mut rng);
            ids.push(w.id);
            nids.push(w.randomnumber);
            w
        })
        .collect();

    // Update the random worlds in the database.
    client.execute(update, &[&ids, &nids]).await.unwrap();

    (StatusCode::OK, Json(worlds))
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url: String = get_env("POSTGRES_URL");
    let max_pool_size: u32 = get_env("POSTGRES_MAX_POOL_SIZE");

    let pool = create_pool(database_url, max_pool_size).await;

    let app = Router::new()
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .route("/queries", get(queries))
        .route("/updates", get(updates))
        .with_state(pool);

    server::serve_hyper(app, Some(8000)).await
}
