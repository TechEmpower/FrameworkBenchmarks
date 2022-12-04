use std::{
    convert::identity,
    sync::Arc,
    thread::{available_parallelism, spawn},
};

use nanorand::{Rng, WyRand};
use once_cell::sync::OnceCell;
use viz::{
    header::SERVER, types::State, Error, Request, RequestExt, Response, ResponseExt,
    Result, Router, ServiceMaker,
};
use yarte::ywrite_html;

mod db_pg;
mod models;
mod server;
mod utils;

use db_pg::{PgConnection, PgError};
use utils::{HDR_SERVER, RANGE};

const DB_URL: &str =
    "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
static CACHED: OnceCell<Vec<models::World>> = OnceCell::new();

async fn db(req: Request) -> Result<Response> {
    let db = req.state::<Arc<PgConnection>>().ok_or(PgError::Connect)?;

    let world = db.get_world().await?;

    let mut res = Response::json(world)?;
    res.headers_mut().insert(SERVER, HDR_SERVER);
    Ok(res)
}

async fn fortunes(req: Request) -> Result<Response> {
    let db = req.state::<Arc<PgConnection>>().ok_or(PgError::Connect)?;

    let fortunes = db.tell_fortune().await?;

    let mut buf = String::with_capacity(2048);
    ywrite_html!(buf, "{{> fortune }}");

    let mut res = Response::html(buf);
    res.headers_mut().insert(SERVER, HDR_SERVER);
    Ok(res)
}

async fn queries(req: Request) -> Result<Response> {
    let db = req.state::<Arc<PgConnection>>().ok_or(PgError::Connect)?;

    let count = utils::get_query_param(req.query_string());
    let worlds = db.get_worlds(count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, HDR_SERVER);
    Ok(res)
}

async fn cached_queries(req: Request) -> Result<Response> {
    let count = utils::get_query_param(req.query_string());
    let mut rng = WyRand::new();

    let worlds = (0..count)
        .map(|_| {
            let id = rng.generate_range(RANGE) as usize;
            CACHED.get()?.get(id)
        })
        .filter_map(identity)
        .collect::<Vec<_>>();

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, HDR_SERVER);
    Ok(res)
}

async fn updates(req: Request) -> Result<Response> {
    let db = req.state::<Arc<PgConnection>>().ok_or(PgError::Connect)?;

    let count = utils::get_query_param(req.query_string());
    let worlds = db.update(count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, HDR_SERVER);
    Ok(res)
}

async fn populate_cache() -> Result<(), Error> {
    let conn = PgConnection::connect(DB_URL).await;
    let worlds = conn.get_worlds_by_limit(10_000).await?;
    CACHED.set(worlds).unwrap();
    Ok(())
}

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    rt.block_on(async {
        populate_cache().await.expect("cache insert failed");
    });

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
        .with(State::new(Arc::new(conn)))
        .get("/cached_queries", cached_queries);

    server::builder()
        .serve(ServiceMaker::from(app))
        .await
        .unwrap()
}
