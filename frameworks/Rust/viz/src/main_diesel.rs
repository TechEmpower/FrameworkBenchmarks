#[macro_use]
extern crate diesel;

use std::{
    convert::identity,
    thread::{available_parallelism, spawn},
};

use diesel_async::{
    pooled_connection::{deadpool::Pool, AsyncDieselConnectionManager},
    AsyncPgConnection,
};
use nanorand::{Rng, WyRand};
use once_cell::sync::OnceCell;
use viz::{
    header::{HeaderValue, SERVER},
    types::State,
    Request, RequestExt, Response, ResponseExt, Result, Router, ServiceMaker,
};

mod db_diesel;
pub mod models_diesel;
pub mod schema;
mod server;
mod utils;

use db_diesel::*;
use models_diesel::World;
use utils::RANGE;

const DB_URL: &str =
    "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
static CACHED: OnceCell<Vec<World>> = OnceCell::new();

async fn db(req: Request) -> Result<Response> {
    let mut rng = req.state::<WyRand>().unwrap();
    let pool = req.state::<Pool<AsyncPgConnection>>();

    let random_id = rng.generate_range(RANGE);

    let world = get_world(pool, random_id).await?;

    let mut res = Response::json(world)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn fortunes(req: Request) -> Result<Response> {
    let pool = req.state::<Pool<AsyncPgConnection>>();

    let fortunes = tell_fortune(pool).await?;

    let mut res = Response::html(fortunes);
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn queries(req: Request) -> Result<Response> {
    let rng = req.state::<WyRand>().unwrap();
    let pool = req.state::<Pool<AsyncPgConnection>>();
    let count = utils::get_query_param(req.query_string());

    let worlds = get_worlds(pool, rng, count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
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
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn updates(req: Request) -> Result<Response> {
    let rng = req.state::<WyRand>().unwrap();
    let pool = req.state::<Pool<AsyncPgConnection>>();
    let count = utils::get_query_param(req.query_string());

    let worlds = update_worlds(pool, rng, count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn populate_cache(pool: Option<Pool<AsyncPgConnection>>) -> Result<()> {
    let worlds = get_worlds_by_limit(pool, 10_000).await?;
    CACHED.set(worlds).unwrap();
    Ok(())
}

fn main() {
    let max = available_parallelism().map(|n| n.get()).unwrap_or(16) as usize;

    let pool =
        Pool::<AsyncPgConnection>::builder(AsyncDieselConnectionManager::new(DB_URL))
            .max_size(max)
            .wait_timeout(None)
            .create_timeout(None)
            .recycle_timeout(None)
            .build()
            .unwrap();

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    rt.block_on(populate_cache(Some(pool.clone())))
        .expect("cache insert failed");

    let rng = WyRand::new();

    let service = ServiceMaker::from(
        Router::new()
            .get("/db", db)
            .get("/fortunes", fortunes)
            .get("/queries", queries)
            .get("/updates", updates)
            .with(State::new(pool))
            .get("/cached_queries", cached_queries)
            .with(State::new(rng)),
    );

    for _ in 1..max {
        let service = service.clone();
        spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve(service));
        });
    }

    rt.block_on(serve(service));
}

async fn serve(service: ServiceMaker) {
    server::builder().serve(service).await.unwrap()
}
