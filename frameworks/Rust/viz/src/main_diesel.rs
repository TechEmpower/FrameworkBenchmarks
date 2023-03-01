#[macro_use]
extern crate diesel;

use std::{convert::identity, thread::available_parallelism};

use diesel_async::{
    pooled_connection::{bb8::Pool, AsyncDieselConnectionManager},
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
    let pool = req.state::<Pool<AsyncPgConnection>>().unwrap();

    let random_id = rng.generate_range(RANGE);

    let world = get_world(pool, random_id).await?;

    let mut res = Response::json(world)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn fortunes(req: Request) -> Result<Response> {
    let pool = req.state::<Pool<AsyncPgConnection>>().unwrap();

    let fortunes = tell_fortune(pool).await?;

    let mut res = Response::html(fortunes);
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn queries(req: Request) -> Result<Response> {
    let rng = req.state::<WyRand>().unwrap();
    let pool = req.state::<Pool<AsyncPgConnection>>().unwrap();
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
    let pool = req.state::<Pool<AsyncPgConnection>>().unwrap();
    let count = utils::get_query_param(req.query_string());

    let worlds = update_worlds(pool, rng, count).await?;

    let mut res = Response::json(worlds)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn populate_cache(pool: Pool<AsyncPgConnection>) -> Result<()> {
    let worlds = get_worlds_by_limit(pool, 10_000).await?;
    CACHED.set(worlds).unwrap();
    Ok(())
}

#[tokio::main]
async fn main() {
    let max = available_parallelism().map(|n| n.get()).unwrap_or(16) as u32;

    let pool = Pool::<AsyncPgConnection>::builder()
        .max_size(max)
        .min_idle(Some(max))
        .idle_timeout(None)
        .build_unchecked(AsyncDieselConnectionManager::new(DB_URL));

    populate_cache(pool.clone())
        .await
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

    serve(service).await;
}

async fn serve(service: ServiceMaker) {
    server::builder().serve(service).await.unwrap()
}
