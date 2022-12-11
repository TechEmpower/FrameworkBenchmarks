mod database_pg_pool;
mod models_common;
mod models_pg_pool;
mod server;
mod utils;

use crate::database_pg_pool::{
    create_pool, fetch_all_fortunes, fetch_world_by_id,
    prepare_fetch_all_fortunes_statement, prepare_fetch_world_by_id_statement,
    prepare_update_world_by_id_statement, update_world, DatabaseClient, PgError,
};
use axum::http::{header, HeaderValue};
use axum::{
    extract::Query, http::StatusCode, response::IntoResponse, routing::get, Extension,
    Json, Router,
};
use dotenv::dotenv;
use futures_util::stream::FuturesUnordered;
use futures_util::TryStreamExt;
use rand::rngs::SmallRng;
use rand::{thread_rng, Rng, SeedableRng};
use tower_http::set_header::SetResponseHeaderLayer;
use yarte::Template;

use crate::utils::{get_environment_variable, Utf8Html};
use models_pg_pool::{Fortune, World};
use utils::{parse_params, random_number, Params};

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

async fn db(DatabaseClient(client): DatabaseClient) -> impl IntoResponse {
    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();

    let random_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

    let select = prepare_fetch_world_by_id_statement(&client).await;
    let world = fetch_world_by_id(&client, random_id, &select)
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

    let select = prepare_fetch_world_by_id_statement(&client).await;

    let future_worlds = FuturesUnordered::new();

    for _ in 0..q {
        let w_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

        future_worlds.push(fetch_world_by_id(&client, w_id, &select));
    }

    let worlds: Result<Vec<World>, PgError> = future_worlds.try_collect().await;
    let results = worlds.expect("worlds could not be retrieved");

    (StatusCode::OK, Json(results))
}

async fn fortunes(DatabaseClient(client): DatabaseClient) -> impl IntoResponse {
    let select = prepare_fetch_all_fortunes_statement(&client).await;

    let mut fortunes = fetch_all_fortunes(client, &select)
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

    let select = prepare_fetch_world_by_id_statement(&client).await;

    let future_worlds = FuturesUnordered::new();

    for _ in 0..q {
        let query_id = random_number(&mut rng);

        future_worlds.push(fetch_world_by_id(&client, query_id, &select));
    }

    let worlds: Result<Vec<World>, PgError> = future_worlds.try_collect().await;
    let results = worlds.expect("worlds could not be retrieved");

    let update = prepare_update_world_by_id_statement(&client).await;

    let future_world_updates = FuturesUnordered::new();

    for w in &results {
        let random_id = random_number(&mut rng);
        let w_id = w.id;

        future_world_updates.push(update_world(&client, &update, random_id, w_id));
    }

    let world_updates: Result<Vec<u64>, PgError> =
        future_world_updates.try_collect().await;
    world_updates.expect("updates could not be executed");

    (StatusCode::OK, Json(results))
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    serve().await;
}

async fn serve() {
    let database_url: String = get_environment_variable("AXUM_TECHEMPOWER_DATABASE_URL");
    let max_pool_size: u32 = get_environment_variable("AXUM_TECHEMPOWER_MAX_POOL_SIZE");

    // setup Client pool
    let pool = create_pool(database_url, max_pool_size).await;
    let server_header_value = HeaderValue::from_static("Axum");

    let router = Router::new()
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .route("/queries", get(queries))
        .route("/updates", get(updates))
        .layer(Extension(pool))
        .layer(SetResponseHeaderLayer::if_not_present(
            header::SERVER,
            server_header_value,
        ));

    server::builder()
        .serve(router.into_make_service())
        .await
        .unwrap();
}
