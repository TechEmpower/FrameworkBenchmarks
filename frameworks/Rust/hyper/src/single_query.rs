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

static QUERY: &str = "SELECT id, randomnumber FROM world WHERE id = $1";

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

pub async fn get() -> Result<Response<BoxBody<Bytes, Infallible>>> {
    let id = fastrand::i32(1..10_000);
    let world = query_world(id).await?;
    let json = serde_json::to_vec(&world)?;

    Response::builder()
        .header(SERVER, SERVER_HEADER.clone())
        .header(CONTENT_TYPE, APPLICATION_JSON.clone())
        .header(CONTENT_LENGTH, json.len())
        .body(Full::from(json).boxed())
        .map_err(Error::from)
}

async fn query_world(id: i32) -> Result<World, Error> {
    let db = POOL.get().await?;
    let statement = db.prepare_cached(QUERY).await?;
    let row = db.query_one(&statement, &[&id]).await?;
    let world = World::from(row);
    Ok(world)
}
