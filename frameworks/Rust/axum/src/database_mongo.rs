use axum::async_trait;
use axum::extract::{Extension, FromRequest, RequestParts};
use axum::http::StatusCode;
use futures_util::stream::FuturesUnordered;
use futures_util::TryStreamExt;
use std::io;

use crate::utils::internal_error;
use crate::{Fortune, World};
use futures_util::StreamExt;
use mongodb::bson::doc;
use mongodb::Database;

pub struct DatabaseConnection(pub Database);

#[async_trait]
impl<B> FromRequest<B> for DatabaseConnection
where
    B: Send,
{
    type Rejection = (StatusCode, String);

    async fn from_request(req: &mut RequestParts<B>) -> Result<Self, Self::Rejection> {
        let Extension(db) = Extension::<Database>::from_request(req)
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
    let world_collection = db.collection::<World>("world");

    let filter = doc! { "_id": id as f32 };

    let world: World = world_collection
        .find_one(Some(filter), None)
        .await
        .unwrap()
        .expect("expected world, found none");
    Ok(world)
}

pub async fn find_worlds(db: Database, ids: Vec<i32>) -> Result<Vec<World>, MongoError> {
    let future_worlds = FuturesUnordered::new();

    for id in ids {
        future_worlds.push(find_world_by_id(db.clone(), id));
    }

    let worlds: Result<Vec<World>, MongoError> = future_worlds.try_collect().await;
    worlds
}

pub async fn fetch_fortunes(db: Database) -> Result<Vec<Fortune>, MongoError> {
    let fortune_collection = db.collection::<Fortune>("fortune");

    let mut fortune_cursor = fortune_collection
        .find(None, None)
        .await
        .expect("fortunes could not be loaded");

    let mut fortunes: Vec<Fortune> = Vec::new();

    while let Some(doc) = fortune_cursor.next().await {
        fortunes.push(doc.expect("could not load fortune"));
    }

    fortunes.push(Fortune {
        id: 0.0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));
    Ok(fortunes)
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
