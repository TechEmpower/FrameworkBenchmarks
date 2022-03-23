use crate::db::{
    world::{Entity as Worlds, Model as World},
    DbConnExt,
};

use futures_util::stream::{futures_unordered::FuturesUnordered, StreamExt};
use sea_orm::{entity::prelude::*, IntoActiveModel, Set};
use std::iter;
use trillium::Conn;
use trillium_api::ApiConnExt;
use trillium_router::RouterConnExt;

pub async fn handler(conn: Conn) -> Conn {
    let queries = conn
        .param("updates")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1)
        .min(500)
        .max(1);

    let db = conn.db();

    let vec_of_worlds: Vec<World> =
        iter::repeat_with(|| Worlds::find_by_id(fastrand::i32(1..10000)).one(db))
            .take(queries)
            .collect::<FuturesUnordered<_>>()
            .filter_map(|x| async move { x.ok().flatten() })
            .filter_map(|w| async move {
                let mut am = w.clone().into_active_model();
                am.random_number = Set(fastrand::i32(1..10000));
                am.update(db).await.ok()
            })
            .collect()
            .await;

    conn.with_json(&vec_of_worlds)
}
