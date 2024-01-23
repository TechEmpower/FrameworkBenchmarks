use crate::db::{world::Entity as Worlds, DbConnExt};
use futures_lite::StreamExt;
use sea_orm::{entity::prelude::*, TransactionTrait};
use std::iter;
use trillium::{Conn, Status};
use trillium_api::ApiConnExt;
use trillium_router::RouterConnExt;
use unicycle::FuturesUnordered;

pub async fn handler(conn: Conn) -> Conn {
    let queries = conn
        .param("queries")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1)
        .min(500)
        .max(1);

    let Ok(tx) = conn.db().begin().await else {
        return conn.with_status(Status::InternalServerError);
    };

    let worlds = iter::repeat_with(|| async {
        Worlds::find_by_id(fastrand::i32(1..=10_000))
            .one(&tx)
            .await?
            .ok_or_else(|| DbErr::RecordNotFound(String::from("not found")))
    })
    .take(queries)
    .collect::<FuturesUnordered<_>>()
    .try_collect::<_, _, Vec<_>>()
    .await;

    match worlds {
        Ok(worlds) => conn.with_json(&worlds),
        Err(_) => conn.with_status(Status::InternalServerError),
    }
}
