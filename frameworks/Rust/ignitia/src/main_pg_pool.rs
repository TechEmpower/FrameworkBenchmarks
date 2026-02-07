mod common;
mod pg_pool;

use common::{
    SELECT_ALL_FORTUNES, SELECT_WORLD_BY_ID, UPDATE_WORLDS, get_env, random_id, random_ids,
    utils::{Params, parse_params},
};
use futures_util::{TryStreamExt, stream::FuturesUnordered};
use ignitia::{Query, Response, Router, Server, State};
use pg_pool::database::{create_pool, fetch_all_fortunes, fetch_world_by_id};
use pg_pool::models::{Fortune, World};
use rand::{SeedableRng, rng, rngs::SmallRng};
use yarte::Template;

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

async fn db(State(pool): State<deadpool_postgres::Pool>) -> Response {
    let random_id = random_id(&mut rng());

    match pool.get().await {
        Ok(client) => {
            let select = client.prepare_cached(SELECT_WORLD_BY_ID).await.unwrap();
            match fetch_world_by_id(&client, random_id, &select).await {
                Ok(world) => Response::json(world)
                    .with_header("Server", "Ignitia")
                    .with_header("Content-Type", "application/json")
                    .with_header(
                        "Date",
                        httpdate::fmt_http_date(std::time::SystemTime::now()),
                    ),
                Err(_) => Response::internal_error()
                    .with_header("Server", "Ignitia")
                    .with_header(
                        "Date",
                        httpdate::fmt_http_date(std::time::SystemTime::now()),
                    ),
            }
        }
        Err(_) => Response::internal_error()
            .with_header("Server", "Ignitia")
            .with_header(
                "Date",
                httpdate::fmt_http_date(std::time::SystemTime::now()),
            ),
    }
}

async fn queries(
    State(pool): State<deadpool_postgres::Pool>,
    Query(params): Query<Params>,
) -> Response {
    let q = parse_params(params);
    let mut rng = SmallRng::from_rng(&mut rng());

    match pool.get().await {
        Ok(client) => {
            let select = client.prepare_cached(SELECT_WORLD_BY_ID).await.unwrap();
            let future_worlds = FuturesUnordered::new();

            for id in random_ids(&mut rng, q) {
                future_worlds.push(fetch_world_by_id(&client, id, &select));
            }

            match future_worlds.try_collect::<Vec<World>>().await {
                Ok(results) => Response::json(results)
                    .with_header("Server", "Ignitia")
                    .with_header("Content-Type", "application/json")
                    .with_header(
                        "Date",
                        httpdate::fmt_http_date(std::time::SystemTime::now()),
                    ),
                Err(_) => Response::internal_error()
                    .with_header("Server", "Ignitia")
                    .with_header(
                        "Date",
                        httpdate::fmt_http_date(std::time::SystemTime::now()),
                    ),
            }
        }
        Err(_) => Response::internal_error()
            .with_header("Server", "Ignitia")
            .with_header(
                "Date",
                httpdate::fmt_http_date(std::time::SystemTime::now()),
            ),
    }
}

async fn fortunes(State(pool): State<deadpool_postgres::Pool>) -> Response {
    match pool.get().await {
        Ok(client) => {
            let select = client.prepare_cached(SELECT_ALL_FORTUNES).await.unwrap();

            match fetch_all_fortunes(client, &select).await {
                Ok(mut fortunes) => {
                    fortunes.push(Fortune {
                        id: 0,
                        message: "Additional fortune added at request time.".to_string(),
                    });

                    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

                    let html = FortunesTemplate {
                        fortunes: &fortunes,
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
                    .with_header(
                        "Date",
                        httpdate::fmt_http_date(std::time::SystemTime::now()),
                    ),
            }
        }
        Err(_) => Response::internal_error()
            .with_header("Server", "Ignitia")
            .with_header(
                "Date",
                httpdate::fmt_http_date(std::time::SystemTime::now()),
            ),
    }
}

async fn updates(
    State(pool): State<deadpool_postgres::Pool>,
    Query(params): Query<Params>,
) -> Response {
    let q = parse_params(params);
    let mut rng = SmallRng::from_rng(&mut rng());

    match pool.get().await {
        Ok(client) => {
            let select = client.prepare_cached(SELECT_WORLD_BY_ID).await.unwrap();
            let update = client.prepare_cached(UPDATE_WORLDS).await.unwrap();

            let future_worlds = FuturesUnordered::new();
            for id in random_ids(&mut rng, q) {
                future_worlds.push(fetch_world_by_id(&client, id, &select));
            }

            match future_worlds.try_collect::<Vec<World>>().await {
                Ok(worlds) => {
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

                    match client.execute(&update, &[&ids, &nids]).await {
                        Ok(_) => Response::json(worlds)
                            .with_header("Server", "Ignitia")
                            .with_header("Content-Type", "application/json")
                            .with_header(
                                "Date",
                                httpdate::fmt_http_date(std::time::SystemTime::now()),
                            ),
                        Err(_) => Response::internal_error()
                            .with_header("Server", "Ignitia")
                            .with_header(
                                "Date",
                                httpdate::fmt_http_date(std::time::SystemTime::now()),
                            ),
                    }
                }
                Err(_) => Response::internal_error()
                    .with_header("Server", "Ignitia")
                    .with_header(
                        "Date",
                        httpdate::fmt_http_date(std::time::SystemTime::now()),
                    ),
            }
        }
        Err(_) => Response::internal_error()
            .with_header("Server", "Ignitia")
            .with_header(
                "Date",
                httpdate::fmt_http_date(std::time::SystemTime::now()),
            ),
    }
}

#[tokio::main]
async fn main() {
    dotenv::dotenv().ok();

    let database_url: String = get_env("POSTGRES_URL");
    let max_pool_size: u32 = get_env("POSTGRES_MAX_POOL_SIZE");

    let pool = create_pool(database_url, max_pool_size).await;

    let app = Router::new()
        .get("/db", db)
        .get("/queries", queries)
        .get("/fortunes", fortunes)
        .get("/updates", updates)
        .state(pool);

    println!("Starting Ignitia PostgreSQL (deadpool) server on 0.0.0.0:8000");

    Server::new(app, "0.0.0.0:8000".parse().unwrap())
        .with_performance_config(ignitia::PerformanceConfig::max_rps())
        .ignitia()
        .await
        .unwrap();
}
