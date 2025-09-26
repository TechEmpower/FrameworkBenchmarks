use crate::db::{world::Entity as Worlds, DbConnExt};
use futures_lite::StreamExt;
use futures_util::stream::futures_unordered::FuturesUnordered;
use sea_orm::entity::prelude::*;
use std::iter;
use trillium::{conn_try, Conn};
use trillium_api::ApiConnExt;
use trillium_router::RouterConnExt;

pub async fn handler(conn: Conn) -> Conn {
    let queries = conn
        .param("queries")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1)
        .min(500)
        .max(1);

    let db = conn.db();

    let vec_of_worlds: Result<Vec<_>, DbErr> =
        iter::repeat_with(|| Worlds::find_by_id(fastrand::i32(1..10000)).one(db))
            .take(queries)
            .collect::<FuturesUnordered<_>>()
            .map(|x| match x {
                Ok(None) => Err(DbErr::RecordNotFound(String::from("not found"))),
                other => other,
            })
            .try_collect()
            .await;

    let vec_of_worlds = conn_try!(vec_of_worlds, conn);

    conn.with_json(&vec_of_worlds)
}
