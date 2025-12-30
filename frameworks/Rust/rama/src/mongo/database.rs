use std::{convert::Infallible, io};

use futures_util::{StreamExt, TryStreamExt, stream::FuturesUnordered};
use mongodb::{Database, bson::doc};
use rama::{
    Context,
    http::{dep::http::request, service::web::extract::FromRequestContextRefPair},
};
use rand::rngs::SmallRng;

use crate::common::{
    models::{Fortune, World},
    random_ids,
};

pub struct DatabaseConnection(pub Database);

impl FromRequestContextRefPair<Database> for DatabaseConnection {
    type Rejection = Infallible;

    async fn from_request_context_ref_pair(
        ctx: &Context<Database>,
        _parts: &request::Parts,
    ) -> Result<Self, Self::Rejection> {
        Ok(Self(ctx.state_clone()))
    }
}

#[derive(Debug)]
#[allow(dead_code)]
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
        .find_one(filter)
        .await
        .unwrap()
        .expect("expected world, found none");
    Ok(world)
}

pub async fn find_worlds(
    db: Database,
    rng: &mut SmallRng,
    count: usize,
) -> Result<Vec<World>, MongoError> {
    let future_worlds = FuturesUnordered::new();

    for id in random_ids(rng, count) {
        future_worlds.push(find_world_by_id(db.clone(), id));
    }

    let worlds: Result<Vec<World>, MongoError> = future_worlds.try_collect().await;
    worlds
}

pub async fn fetch_fortunes(db: Database) -> Result<Vec<Fortune>, MongoError> {
    let fortune_collection = db.collection::<Fortune>("fortune");

    let mut fortune_cursor = fortune_collection
        .find(doc! {})
        .await
        .expect("fortunes could not be loaded");

    let mut fortunes: Vec<Fortune> = Vec::new();

    while let Some(doc) = fortune_cursor.next().await {
        fortunes.push(doc.expect("could not load fortune"));
    }

    fortunes.push(Fortune {
        id: 0,
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

    db.run_command(doc! {"update": "world", "updates": updates, "ordered": false})
        .await
        .expect("could not update worlds");

    Ok(true)
}
