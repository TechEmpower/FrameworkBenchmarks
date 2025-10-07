mod common;
mod pg;

use common::{
    get_env, random_id,
    utils::{Params, parse_params},
};
use ignitia::{Query, Response, Router, Server, State};
use pg::database::PgConnection;
use pg::models::Fortune;
use rand::rng;
use yarte::Template;

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

async fn db(State(conn): State<std::sync::Arc<PgConnection>>) -> Response {
    let id = random_id(&mut rng());
    match conn.fetch_world_by_id(id).await {
        Ok(world) => Response::json(world)
            .with_header("Server", "Ignitia")
            .with_header("Content-Type", "application/json")
            .with_header(
                "Date",
                httpdate::fmt_http_date(std::time::SystemTime::now()),
            ),
        Err(_) => Response::internal_error(),
    }
}

async fn queries(
    State(conn): State<std::sync::Arc<PgConnection>>,
    Query(params): Query<Params>,
) -> Response {
    let q = parse_params(params);
    match conn.fetch_random_worlds(q).await {
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

async fn fortunes(State(conn): State<std::sync::Arc<PgConnection>>) -> Response {
    match conn.fetch_all_fortunes().await {
        Ok(fortunes) => {
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
            .with_header("Date", httpdate::fmt_http_date(std::time::SystemTime::now())),
    }
}

async fn updates(
    State(conn): State<std::sync::Arc<PgConnection>>,
    Query(params): Query<Params>,
) -> Response {
    let q = parse_params(params);
    match conn.update_worlds(q).await {
        Ok(worlds) => Response::json(worlds)
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

#[tokio::main]
async fn main() {
    dotenv::dotenv().ok();

    let database_url: String = get_env("POSTGRES_URL");
    let pg_connection = PgConnection::connect(database_url).await;

    let app = Router::new()
        .get("/db", db)
        .get("/queries", queries)
        .get("/fortunes", fortunes)
        .get("/updates", updates)
        .state(pg_connection);

    Server::new(app, "0.0.0.0:8000".parse().unwrap())
        .with_performance_config(ignitia::PerformanceConfig::max_rps())
        .ignitia()
        .await
        .unwrap();
}
