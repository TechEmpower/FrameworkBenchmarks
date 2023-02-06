use axum::async_trait;
use axum::extract::{Extension, FromRequestParts};
use axum::http::request::Parts;
use axum::http::StatusCode;
use futures_util::stream::FuturesUnordered;
use futures_util::TryStreamExt;
use std::io;

use crate::utils::internal_error;
use crate::World;
use mongodb::bson::{doc, RawDocumentBuf};
use mongodb::Database;

pub struct DatabaseConnection(pub Database);

#[async_trait]
impl<S> FromRequestParts<S> for DatabaseConnection
where
    S: Send + Sync,
{
    type Rejection = (StatusCode, String);

    async fn from_request_parts(
        parts: &mut Parts,
        state: &S,
    ) -> Result<Self, Self::Rejection> {
        let Extension(db) = Extension::<Database>::from_request_parts(parts, state)
            .await
            .map_err(internal_error)?;

        Ok(Self(db))
    }
}

#[derive(Debug)]
pub enum MongoError {
    Io(io::Error),
    Mongo(mongodb::error::Error),
}

impl From<io::Error> for MongoError {
    fn from(err: io::Error) -> Self {
        MongoError::Io(err)
    }
}

impl From<mongodb::error::Error> for MongoError {
    fn from(err: mongodb::error::Error) -> Self {
        MongoError::Mongo(err)
    }
}

pub async fn find_world_by_id(db: Database, id: i32) -> Result<World, MongoError> {
    let world_collection = db.collection::<RawDocumentBuf>("world");

    let filter = doc! { "_id": id as f32 };

    let raw: RawDocumentBuf = world_collection
        .find_one(Some(filter), None)
        .await
        .unwrap()
        .expect("expected world, found none");

    Ok(World {
        id: raw
            .get("id")
            .expect("expected to parse world id")
            .expect("could not get world id")
            .as_i32()
            .expect("could not extract world id"),
        random_number: raw
            .get("id")
            .expect("expected to parse world id")
            .expect("could not get world id")
            .as_i32()
            .expect("could not extract world id"),
    })
}

pub async fn find_worlds(db: Database, ids: Vec<i32>) -> Result<Vec<World>, MongoError> {
    let future_worlds = FuturesUnordered::new();

    for id in ids {
        future_worlds.push(find_world_by_id(db.clone(), id));
    }

    let worlds: Result<Vec<World>, MongoError> = future_worlds.try_collect().await;
    worlds
}

pub async fn update_worlds(
    db: Database,
    worlds: Vec<World>,
) -> Result<bool, MongoError> {
    let mut updates = Vec::new();

    for world in worlds {
        updates.push(doc! {
        "q": { "id": world.id }, "u": { "$set": { "randomNumber": world.random_number }}
        });
    }

    db.run_command(
        doc! {"update": "world", "updates": updates, "ordered": false},
        None,
    )
    .await
    .expect("could not update worlds");

    Ok(true)
}
