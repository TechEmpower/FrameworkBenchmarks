use crate::db::{world::Entity as World, DbConnExt};
use sea_orm::entity::prelude::*;
use trillium::{conn_unwrap, Conn};
use trillium_api::ApiConnExt;

pub async fn handler(conn: Conn) -> Conn {
    let random = fastrand::i32(1..10000);
    let world = conn_unwrap!(
        World::find_by_id(random)
            .one(conn.db())
            .await
            .ok()
            .flatten(),
        conn
    );

    conn.with_json(&world)
}
