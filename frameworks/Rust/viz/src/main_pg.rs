use std::{
    convert::identity,
    sync::Arc,
    thread::{available_parallelism, spawn},
};

use nanorand::{Rng, WyRand};
use viz::{
    header::{HeaderValue, SERVER},
    types::State,
    Request, RequestExt, Response, ResponseExt, Result, Router, ServiceMaker,
};
use yarte::ywrite_html;

mod db_pg;
mod models;
mod server;
mod utils;

use db_pg::{get_conn, PgConnection};
use utils::RANGE;

const DB_URL: &str =
    "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

async fn db(req: Request) -> Result<Response> {
    let conn = get_conn(req.state::<Arc<PgConnection>>())?;

    let world = conn.get_world().await?;

    let mut res = Response::json(world)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn fortunes(req: Request) -> Result<Response> {
    let conn = get_conn(req.state::<Arc<PgConnection>>())?;

    let fortunes = conn.tell_fortune().await?;

    let mut buf = String::with_capacity(2048);
    ywrite_html!(buf, "{{> fortune }}");

    let mut res = Response::html(buf);
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn queries(req: Request) -> Result<Response> {
    let count = utils::get_query_param(req.query_string());
    let conn = get_conn(req.state::<Arc<PgConnection>>())?;

    let worlds = conn.get_worlds(count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn updates(req: Request) -> Result<Response> {
    let count = utils::get_query_param(req.query_string());
    let conn = get_conn(req.state::<Arc<PgConnection>>())?;

    let worlds = conn.update(count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    for _ in 1..available_parallelism().map(|n| n.get()).unwrap_or(16) {
        spawn(move || {
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
    let conn = PgConnection::connect(DB_URL).await;

    let app = Router::new()
        .get("/db", db)
        .get("/fortunes", fortunes)
        .get("/queries", queries)
        .get("/updates", updates)
        .with(State::new(Arc::new(conn)));

    server::builder()
        .serve(ServiceMaker::from(app))
        .await
        .unwrap()
}
