use std::convert::Infallible;

use http::header::{CONTENT_LENGTH, CONTENT_TYPE, SERVER};
use http::Response;
use http_body_util::combinators::BoxBody;
use http_body_util::{BodyExt, Full};
use hyper::body::Bytes;
use serde::Serialize;
use tokio_postgres::Row;

use crate::db::POOL;
use crate::{Error, Result, APPLICATION_JSON, SERVER_HEADER};

const QUERY: &str = "SELECT id, randomnumber FROM world WHERE id = $1";

#[derive(Debug, Serialize)]
pub struct World {
    id: i32,
    randomnumber: i32,
}

impl From<Row> for World {
    fn from(row: Row) -> Self {
        World {
            id: row.get(0),
            randomnumber: row.get(1),
        }
    }
}

pub async fn get(query: Option<&str>) -> Result<Response<BoxBody<Bytes, Infallible>>> {
    let count = query
        .and_then(|query| query.strip_prefix("count="))
        .and_then(|query| query.parse().ok())
        .unwrap_or(1)
        .clamp(1, 500);

    let worlds = query_worlds(count).await?;
    let json = serde_json::to_vec(&worlds)?;

    Response::builder()
        .header(SERVER, SERVER_HEADER.clone())
        .header(CONTENT_TYPE, APPLICATION_JSON.clone())
        .header(CONTENT_LENGTH, json.len())
        .body(Full::from(json).boxed())
        .map_err(Error::from)
}

async fn query_worlds(count: usize) -> Result<Vec<World>> {
    let db = POOL.get().await?;
    let statement = db.prepare_cached(QUERY).await?;
    let mut worlds = Vec::with_capacity(count);
    for _ in 0..count {
        let id = fastrand::i32(1..10_000);
        let row = db.query_one(&statement, &[&id]).await?;
        worlds.push(World::from(row));
    }
    Ok(worlds)
}
