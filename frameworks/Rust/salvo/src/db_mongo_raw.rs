use futures_util::{stream::FuturesUnordered, TryStreamExt};
use mongodb::{
    bson::{doc, RawDocumentBuf},
    error::Error,
    Database,
};

use crate::World;

pub async fn find_world_by_id(db: Database, id: i32) -> Result<World, Error> {
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

pub async fn find_worlds(db: Database, ids: Vec<i32>) -> Result<Vec<World>, Error> {
    let future_worlds = FuturesUnordered::new();

    for id in ids {
        future_worlds.push(find_world_by_id(db.clone(), id));
    }

    let worlds: Result<Vec<World>, Error> = future_worlds.try_collect().await;
    worlds
}

pub async fn update_worlds(db: Database, worlds: Vec<World>) -> Result<bool, Error> {
    let mut updates = Vec::new();

    for world in worlds {
        updates.push(doc! {
        "q": { "id": world.id }, "u": { "$set": { "randomNumber": world.random_number }}
        });
    }

    db.run_command(doc! {"update": "world", "updates": updates, "ordered": false}, None)
        .await
        .expect("could not update worlds");

    Ok(true)
}
