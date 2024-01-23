use crate::db::{world::Entity as World, DbConnExt};
use sea_orm::entity::prelude::*;
use trillium::{Conn, Status};
use trillium_api::ApiConnExt;

pub async fn handler(conn: Conn) -> Conn {
    let id = fastrand::i32(1..=10_000);
    match World::find_by_id(id).one(conn.db()).await {
        Ok(Some(world)) => conn.with_json(&world),
        Err(_) => conn.with_status(Status::InternalServerError),
        Ok(None) => conn.with_status(Status::NotFound),
    }
}
