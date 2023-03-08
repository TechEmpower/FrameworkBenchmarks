mod database_mongo_raw;
mod models_common;
mod models_mongo;
mod server;
mod utils;

use axum::http::{header, HeaderValue};
use axum::{
    extract::Query, http::StatusCode, response::IntoResponse, routing::get, Extension,
    Json, Router,
};
use dotenv::dotenv;
use mongodb::options::{ClientOptions, Compressor};
use mongodb::Client;
use rand::{rngs::SmallRng, thread_rng, Rng, SeedableRng};
use std::time::Duration;
use tower_http::set_header::SetResponseHeaderLayer;

use database_mongo_raw::DatabaseConnection;
use database_mongo_raw::{find_world_by_id, find_worlds, update_worlds};
use models_mongo::World;
use utils::get_environment_variable;
use utils::{parse_params, Params};

async fn db(DatabaseConnection(db): DatabaseConnection) -> impl IntoResponse {
    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();

    let random_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

    let world = find_world_by_id(db, random_id)
        .await
        .expect("world could not be found");

    (StatusCode::OK, Json(world))
}

async fn queries(
    DatabaseConnection(db): DatabaseConnection,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();
    let mut ids: Vec<i32> = Vec::with_capacity(q as usize);

    for _ in 0..q {
        let random_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

        ids.push(random_id);
    }

    let worlds = find_worlds(db, ids).await;
    let results = worlds.expect("worlds could not be retrieved");

    (StatusCode::OK, Json(results))
}

async fn updates(
    DatabaseConnection(db): DatabaseConnection,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let mut rng = SmallRng::from_rng(&mut thread_rng()).unwrap();
    let mut ids: Vec<i32> = Vec::with_capacity(q as usize);

    for _ in 0..q {
        let random_id = (rng.gen::<u32>() % 10_000 + 1) as i32;

        ids.push(random_id);
    }

    let worlds = find_worlds(db.clone(), ids)
        .await
        .expect("worlds could not be retrieved");
    let mut updated_worlds: Vec<World> = Vec::with_capacity(q as usize);

    for mut world in worlds {
        let random_number = (rng.gen::<u32>() % 10_000 + 1) as i32;

        world.random_number = random_number;
        updated_worlds.push(world);
    }

    update_worlds(db.clone(), updated_worlds.clone())
        .await
        .expect("could not update worlds");

    (StatusCode::OK, Json(updated_worlds.clone()))
}

fn main() {
    dotenv().ok();

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    for _ in 1..num_cpus::get() {
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve());
        });
    }
    rt.block_on(serve());
}

async fn serve() {
    let database_url: String = get_environment_variable("AXUM_TECHEMPOWER_MONGODB_URL");
    let max_pool_size: u32 = get_environment_variable("AXUM_TECHEMPOWER_MAX_POOL_SIZE");
    let min_pool_size: u32 = get_environment_variable("AXUM_TECHEMPOWER_MIN_POOL_SIZE");

    let mut client_options = ClientOptions::parse(database_url).await.unwrap();

    // setup connection pool
    client_options.max_pool_size = Some(max_pool_size);
    client_options.min_pool_size = Some(min_pool_size);
    client_options.connect_timeout = Some(Duration::from_millis(200));

    // the server will select the algorithm it supports from the list provided by the driver
    client_options.compressors = Some(vec![
        Compressor::Snappy,
        Compressor::Zlib {
            level: Default::default(),
        },
        Compressor::Zstd {
            level: Default::default(),
        },
    ]);

    let client = Client::with_options(client_options).unwrap();
    let database = client.database("hello_world");
    let server_header_value = HeaderValue::from_static("Axum");

    let app = Router::new()
        .route("/db", get(db))
        .route("/queries", get(queries))
        .route("/updates", get(updates))
        .layer(Extension(database))
        .layer(SetResponseHeaderLayer::if_not_present(
            header::SERVER,
            server_header_value,
        ));

    server::builder()
        .serve(app.into_make_service())
        .await
        .unwrap();
}
