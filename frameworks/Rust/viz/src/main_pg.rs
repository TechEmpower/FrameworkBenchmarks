use std::convert::identity;
use std::sync::Arc;

use nanorand::{Rng, WyRand};
use stretto::AsyncCache;
use viz::{
    header::SERVER, types::State, Error, HandlerExt, Request, RequestExt, Response,
    ResponseExt, Result, Router, ServiceMaker,
};
use yarte::ywrite_html;

mod db_pg;
mod models;
mod server;
mod utils;

use db_pg::{PgConnection, PgError};

async fn db(req: Request) -> Result<Response> {
    let db = req.state::<Arc<PgConnection>>().ok_or(PgError::Connect)?;

    let world = db.get_world().await?;

    let mut res = Response::json(world)?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn fortunes(req: Request) -> Result<Response> {
    let db = req.state::<Arc<PgConnection>>().ok_or(PgError::Connect)?;

    let fortunes = db.tell_fortune().await?;

    let mut buf = String::with_capacity(2048);
    ywrite_html!(buf, "{{> fortune }}");

    let mut res = Response::html(buf);
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn queries(req: Request) -> Result<Response> {
    let db = req.state::<Arc<PgConnection>>().ok_or(PgError::Connect)?;

    let count = utils::get_query_param(req.query_string());
    let worlds = db.get_worlds(count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn cached_queries(req: Request) -> Result<Response> {
    let cahced = req
        .state::<AsyncCache<i32, models::World>>()
        .ok_or(PgError::Connect)?;

    let count = utils::get_query_param(req.query_string());
    let mut rng = WyRand::new();

    let worlds = (0..count)
        .map(|_| {
            let id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            cahced.get(&id).map(|v| v.read())
        })
        .filter_map(identity)
        .collect::<Vec<_>>();

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn updates(req: Request) -> Result<Response> {
    let db = req.state::<Arc<PgConnection>>().ok_or(PgError::Connect)?;

    let count = utils::get_query_param(req.query_string());
    let worlds = db.update(count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

#[tokio::main]
async fn main() -> Result<()> {
    const DB_URL: &str =
        "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    let pg_conn = Arc::new(PgConnection::connect(DB_URL).await);

    let cached = AsyncCache::new(10_000, 1e6 as i64, tokio::spawn).unwrap();

    {
        let worlds = pg_conn.get_worlds_by_limit(10_000).await?;
        for w in worlds {
            cached.insert(w.id, w, 1).await;
        }
        cached.wait().await.expect("cache insert failed");
    }

    let app = Router::new()
        .get("/db", db)
        .get("/fortunes", fortunes)
        .get("/queries", queries)
        .get("/updates", updates)
        .with(State::new(pg_conn))
        .get("/cached_queries", cached_queries.with(State::new(cached)));

    server::builder()
        .serve(ServiceMaker::from(app))
        .await
        .map_err(Error::normal)
}
