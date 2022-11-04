mod database_pg;
mod models_common;
mod models_pg;
mod server;
mod utils;

use axum::http::{header, HeaderValue};
use axum::{
    extract::Query, http::StatusCode, response::IntoResponse, routing::get, Extension,
    Json, Router,
};
use dotenv::dotenv;
use tower_http::set_header::SetResponseHeaderLayer;
use yarte::Template;

use crate::database_pg::{DatabaseConnection, PgConnection};
use models_pg::Fortune;
use utils::{parse_params, Params};

use crate::utils::{get_environment_variable, Utf8Html};

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

async fn db(DatabaseConnection(conn): DatabaseConnection) -> impl IntoResponse {
    let world = conn.get_world().await.expect("error loading world");

    (StatusCode::OK, Json(world))
}

async fn queries(
    DatabaseConnection(conn): DatabaseConnection,
    Query(params): Query<Params>,
) -> impl IntoResponse {
    let q = parse_params(params);

    let results = conn
        .get_worlds(q as usize)
        .await
        .expect("error loading worlds");

    (StatusCode::OK, Json(results))
}

async fn fortunes(DatabaseConnection(conn): DatabaseConnection) -> impl IntoResponse {
    let fortunes: Vec<Fortune> =
        conn.tell_fortune().await.expect("error loading fortunes");

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

    let results = conn.update(q as u16).await.expect("error updating worlds");

    (StatusCode::OK, Json(results))
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
    let database_url: String = get_environment_variable("AXUM_TECHEMPOWER_DATABASE_URL");

    // setup connection pool
    let pg_connection = PgConnection::connect(database_url).await;
    let server_header_value = HeaderValue::from_static("Axum");

    let router = Router::new()
        .route("/fortunes", get(fortunes))
        .route("/db", get(db))
        .route("/queries", get(queries))
        .route("/updates", get(updates))
        .layer(Extension(pg_connection.clone()))
        .layer(SetResponseHeaderLayer::if_not_present(
            header::SERVER,
            server_header_value,
        ));

    server::builder()
        .serve(router.into_make_service())
        .await
        .unwrap();
}
