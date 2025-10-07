mod common;
mod mongo;

use common::{
    get_env,
    models::{FortuneInfo, World},
    random_id,
    utils::{Params, parse_params},
};
use ignitia::{Query, Response, Router, Server, State};
use mongo::database::{fetch_fortunes, find_world_by_id, find_worlds, update_worlds};
use mongodb::{
    Client,
    options::{ClientOptions, Compressor},
};
use rand::{SeedableRng, rng, rngs::SmallRng};
use std::time::Duration;
use yarte::Template;

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<FortuneInfo>,
}

async fn db(State(db): State<mongodb::Database>) -> Response {
    let random_id = random_id(&mut rng());

    match find_world_by_id(db, random_id).await {
        Ok(world) => Response::json(world)
            .with_header("Server", "Ignitia")
            .with_header("Content-Type", "application/json")
            .with_header(
                "Date",
                httpdate::fmt_http_date(std::time::SystemTime::now()),
            ),
        Err(_) => Response::internal_error()
            .with_header("Server", "Ignitia")
            .with_header("Date", httpdate::fmt_http_date(std::time::SystemTime::now())),
    }
}

async fn queries(State(db): State<mongodb::Database>, Query(params): Query<Params>) -> Response {
    let q = parse_params(params);
    let mut rng = SmallRng::from_rng(&mut rng());

    match find_worlds(db, &mut rng, q).await {
        Ok(results) => Response::json(results)
            .with_header("Server", "Ignitia")
            .with_header("Content-Type", "application/json")
            .with_header(
                "Date",
                httpdate::fmt_http_date(std::time::SystemTime::now()),
            ),
        Err(_) => Response::internal_error()
            .with_header("Server", "Ignitia")
            .with_header("Date", httpdate::fmt_http_date(std::time::SystemTime::now())),
    }
}

async fn updates(State(db): State<mongodb::Database>, Query(params): Query<Params>) -> Response {
    let q = parse_params(params);
    let mut rng = SmallRng::from_rng(&mut rng());

    match find_worlds(db.clone(), &mut rng, q).await {
        Ok(worlds) => {
            let mut updated_worlds: Vec<World> = Vec::with_capacity(q);

            for mut world in worlds {
                world.random_number = random_id(&mut rng);
                updated_worlds.push(world);
            }

            match update_worlds(db, updated_worlds.clone()).await {
                Ok(_) => Response::json(updated_worlds)
                    .with_header("Server", "Ignitia")
                    .with_header("Content-Type", "application/json")
                    .with_header(
                        "Date",
                        httpdate::fmt_http_date(std::time::SystemTime::now()),
                    ),
                Err(_) => Response::internal_error()
                    .with_header("Server", "Ignitia")
                    .with_header("Date", httpdate::fmt_http_date(std::time::SystemTime::now())),
            }
        }
        Err(_) => Response::internal_error()
            .with_header("Server", "Ignitia")
            .with_header("Date", httpdate::fmt_http_date(std::time::SystemTime::now())),
    }
}

async fn fortunes(State(db): State<mongodb::Database>) -> Response {
    match fetch_fortunes(db).await {
        Ok(fortune_infos) => {
            let html = FortunesTemplate {
                fortunes: &fortune_infos,
            }
            .call()
            .expect("error rendering template");

            Response::html(html)
                .with_header("Server", "Ignitia")
                .with_header("Content-Type", "text/html; charset=utf-8")
                .with_header(
                    "Date",
                    httpdate::fmt_http_date(std::time::SystemTime::now()),
                )
        }
        Err(_) => Response::internal_error()
            .with_header("Server", "Ignitia")
            .with_header("Date", httpdate::fmt_http_date(std::time::SystemTime::now())),
    }
}

#[tokio::main]
async fn main() {
    dotenv::dotenv().ok();

    let database_url: String = get_env("MONGODB_URL");
    let max_pool_size: u32 = get_env("MONGODB_MAX_POOL_SIZE");
    let min_pool_size: u32 = get_env("MONGODB_MIN_POOL_SIZE");

    let mut client_options = ClientOptions::parse(database_url).await.unwrap();

    client_options.max_pool_size = Some(max_pool_size);
    client_options.min_pool_size = Some(min_pool_size);
    client_options.connect_timeout = Some(Duration::from_millis(200));
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

    let app = Router::new()
        .get("/db", db)
        .get("/queries", queries)
        .get("/updates", updates)
        .get("/fortunes", fortunes)
        .state(database);

    println!("Starting Ignitia MongoDB server on 0.0.0.0:8000");

    Server::new(app, "0.0.0.0:8000".parse().unwrap())
        .with_performance_config(ignitia::PerformanceConfig::max_rps())
        .ignitia()
        .await
        .unwrap();
}
