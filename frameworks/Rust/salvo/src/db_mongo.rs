use futures_util::{stream::FuturesUnordered, StreamExt, TryStreamExt};
use mongodb::{bson::doc, Database, error::Error};

use crate::{Fortune, World};

pub async fn find_world_by_id(db: Database, id: i32) -> Result<World, Error> {
    let world_collection = db.collection::<World>("world");

    let filter = doc! { "_id": id as f32 };

    let world: World = world_collection
        .find_one(Some(filter), None)
        .await
        .unwrap()
        .expect("expected world, found none");
    Ok(world)
}

pub async fn find_worlds(db: Database, ids: Vec<i32>) -> Result<Vec<World>, Error> {
    let future_worlds = FuturesUnordered::new();

    for id in ids {
        future_worlds.push(find_world_by_id(db.clone(), id));
    }

    let worlds: Result<Vec<World>, Error> = future_worlds.try_collect().await;
    worlds
}

pub async fn fetch_fortunes(db: Database) -> Result<Vec<Fortune>, Error> {
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
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));
    Ok(fortunes)
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
