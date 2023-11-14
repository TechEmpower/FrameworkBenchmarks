use std::{convert::Infallible, io};

use axum::{async_trait, extract::FromRequestParts, http::request::Parts};
use futures_util::{stream::FuturesUnordered, TryStreamExt};
use mongodb::{
    bson::{doc, RawDocumentBuf},
    Database,
};

use crate::World;

pub struct DatabaseConnection(pub Database);

#[async_trait]
impl FromRequestParts<Database> for DatabaseConnection {
    type Rejection = Infallible;

    async fn from_request_parts(
        _parts: &mut Parts,
        db: &Database,
    ) -> Result<Self, Self::Rejection> {
        Ok(Self(db.clone()))
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
