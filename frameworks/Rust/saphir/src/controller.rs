use rand::{distributions::{Uniform, Distribution}};
use saphir::prelude::*;
use mongodm::prelude::*;
use futures::{TryStreamExt, stream::FuturesUnordered};
use tokio::task::JoinHandle;
use crate::models::*;
use crate::templates::*;
use crate::cache::*;
use crate::errors::BenchmarkControllerError;

pub static HELLO_WORLD: &'static str = "Hello, world!";

pub struct BenchmarkController {
    db: MongoDatabase,
    worlds: MongoCollection<World>,
    fortunes: MongoCollection<Fortune>,
    cached_worlds: MongoCollection<CachedWorld>,
    range: Uniform<i32>,
}

impl BenchmarkController {
    pub fn new(db: MongoDatabase) -> Self {
        let worlds = db.collection("world");
        let cached_worlds = db.collection("world");
        let fortunes = db.collection("fortune");
        Self {
            db,
            worlds,
            fortunes,
            cached_worlds,
            range: Uniform::from(1..10_001),
        }
    }

    #[inline]
    async fn find_random_world(&self) -> Result<Option<World>, BenchmarkControllerError> {
        let random_id = self.range.sample(&mut rand::thread_rng());
        Ok(self.worlds.find_one(doc! { "_id": random_id }, None).await?)
    }

    #[inline]
    async fn find_random_world_cached(&self) -> Result<Option<CachedWorld>, BenchmarkControllerError> {
        let random_id = self.range.sample(&mut rand::thread_rng());
        find_cached_world_by_id(self.cached_worlds.clone(), random_id).await
    }

    #[inline]
    async fn update_one_random_world(&self) -> Result<(World, JoinHandle<Result<MongoUpdateResult, mongodm::prelude::MongoError>>), BenchmarkControllerError> {
        let mut world = self.find_random_world().await?.ok_or(BenchmarkControllerError::CannotFindRandomWorld)?;
        world.randomNumber = self.range.sample(&mut rand::thread_rng()) as f32;

        let worlds_collection = self.worlds.clone();
        let world_update = world.clone();
        Ok((
            world,
            tokio::spawn(async move {
                worlds_collection.replace_one(doc!{ "_id": world_update.id }, world_update, None).await
            })
        ))
    }

    #[inline]
    async fn update_random_worlds(&self, count: i32, worlds: &mut Vec<World>) -> Result<JoinHandle<Result<BulkUpdateResult, mongodm::prelude::MongoError>>, BenchmarkControllerError> {
        let mut updates = vec![];
        for _ in 0..count {
            if let Some(mut world) = self.find_random_world().await? {
                world.randomNumber = self.range.sample(&mut rand::thread_rng()) as f32;
                updates.push(BulkUpdate {
                    query: doc!{ "_id": world.id },
                    update: doc! { Set: { f!(randomNumber in World): world.randomNumber } },
                    options: None,
                });
                worlds.push(world);
            }
        }

        let worlds_collection = self.worlds.clone();
        let db = self.db.clone();
        let join_handle = tokio::spawn(async move {
            worlds_collection.bulk_update(&db, updates).await
        });
        Ok(join_handle)
    }
}

// The Saphir-idiomatic way of doing this would be to have an empty controller macro, which would
// route all the requests to /<controller_name>/<route>, for example /benchmark/plaintext .
//
// However, in order to expose the API at the root, we use a specifically un-nammed controller.
#[controller(name = "")]
impl BenchmarkController {
    #[get("/plaintext")]
    async fn return_plain(&self) -> &str {
        HELLO_WORLD
    }

    #[get("/json")]
    async fn return_json(&self) -> Json<JsonMessage> {
        Json(JsonMessage { message: HELLO_WORLD })
    }

    #[get("/db")]
    async fn single_query(&self) -> Result<Json<Option<World>>, BenchmarkControllerError> {
        Ok(Json(self.find_random_world().await?))
    }

    #[get("/queries")]
    async fn multiple_queries(&self, queries: Option<String>) -> Result<Json<Vec<World>>, BenchmarkControllerError> {
        let nb_queries: usize = queries.and_then(|q| q.parse::<usize>().ok()).unwrap_or(1).max(1).min(500);

        let mut worlds = Vec::with_capacity(nb_queries);
        for _ in 0..nb_queries {
            if let Some(world) = self.find_random_world().await? {
                worlds.push(world);
            }
        }

        Ok(Json(worlds))
    }

    #[get("/fortunes")]
    async fn fortune(&self) -> Result<FortunesTemplate, BenchmarkControllerError> {
        let mut fortunes: Vec<_> = self.fortunes.find(None, None).await?.try_collect().await?;
        fortunes.push(Fortune {
            id: 0.0,
            message: "Additional fortune added at request time.".to_string(),
        });
        fortunes.sort_unstable_by(|a, b| a.message.cmp(&b.message));
        Ok(FortunesTemplate::new(fortunes))
    }

    #[get("/cached-worlds")]
    async fn cached_queries(&self, count: Option<String>) -> Result<Json<Vec<CachedWorld>>, BenchmarkControllerError> {
        let nb_queries: usize = count.and_then(|q| q.parse::<usize>().ok()).unwrap_or(1).max(1).min(500);

        let mut worlds = Vec::with_capacity(nb_queries);
        for _ in 0..nb_queries {
            if let Some(world) = self.find_random_world_cached().await? {
                worlds.push(world);
            }
        }

        Ok(Json(worlds))
    }

    // Real-world implementation #1
    // Pros: start updating as soon as the first world is queried
    // Cons: Do as many updates as requests
    #[get("/updates")]
    async fn updates(&self, queries: Option<String>) -> Result<Json<Vec<World>>, BenchmarkControllerError> {
        let nb_queries: usize = queries.and_then(|q| q.parse::<usize>().ok()).unwrap_or(1).max(1).min(500);

        let mut futures = FuturesUnordered::new();

        let mut worlds = Vec::with_capacity(nb_queries);
        for _ in 0..nb_queries {
            if let Some(mut world) = self.find_random_world().await? {
                world.randomNumber = self.range.sample(&mut rand::thread_rng()) as f32;

                let worlds_collection = self.worlds.clone();
                let world_update = world.clone();
                futures.push(tokio::spawn(async move {
                    worlds_collection.replace_one(doc!{ "_id": world_update.id }, world_update, None).await
                }));

                worlds.push(world);
            }
        }

        while let Some(r) = futures.try_next().await? { r?; }

        Ok(Json(worlds))
    }

    // Real-world implementation #2
    // Pros: A single bulk update request instead of many updates
    // Cons: only start updating after all data was queried
    #[get("/bulk-updates")]
    async fn bulk_updates(&self, queries: Option<String>) -> Result<Json<Vec<World>>, BenchmarkControllerError> {
        let nb_queries: usize = queries.and_then(|q| q.parse::<usize>().ok()).unwrap_or(1).max(1).min(500);

        let mut worlds = Vec::with_capacity(nb_queries);

        let mut updates = vec![];
        for _ in 0..nb_queries {
            if let Some(mut world) = self.find_random_world().await? {
                world.randomNumber = self.range.sample(&mut rand::thread_rng()) as f32;
                updates.push(BulkUpdate {
                    query: doc!{ "_id": world.id },
                    update: doc! { Set: { f!(randomNumber in World): world.randomNumber } },
                    options: None,
                });
                worlds.push(world);
            }
        }

        self.worlds.bulk_update(&self.db, updates).await?;

        Ok(Json(worlds))
    }

    // Possible alternative bulk update implementation.
    // Not included in the benchmark because it is a bit overkill for a "realistic" approach.
    //
    // Pros: get the pros of both previous methods (start updating quickly and less update requests)
    // Cons: more complex implementation than both previous methods
    #[get("/fast-bulk-updates")]
    async fn fast_bulk_updates(&self, queries: Option<String>) -> Result<Json<Vec<World>>, BenchmarkControllerError> {
        let nb_queries: usize = queries.and_then(|q| q.parse::<usize>().ok()).unwrap_or(1).max(1).min(500);

        let mut worlds = Vec::with_capacity(nb_queries);

        // Submit first update immediately
        let (world, first_fut) = self.update_one_random_world().await?;
        worlds.push(world);

        if nb_queries > 1 {
            let mut futures = FuturesUnordered::new();

            let mut nb_remaining = nb_queries - 1;
            let batches_sizes = [2, 5, 10, 100];

            let mut batch_index = 0;
            for i in 0..batches_sizes.len() {
                batch_index = i;

                let batch_size = batches_sizes[batch_index].min(nb_remaining);
                let fut = self.update_random_worlds(batch_size as i32, &mut worlds).await?;
                futures.push(fut);
                nb_remaining -= batch_size;
                if nb_remaining <= 0 {
                    break;
                }
            }

            while nb_remaining > 0 {
                let batch_size = batches_sizes[batch_index].min(nb_remaining);
                let fut = self.update_random_worlds(batch_size as i32, &mut worlds).await?;
                futures.push(fut);
                nb_remaining -= batch_size;
            }

            while let Some(r) = futures.try_next().await? { r?; }
        }

        first_fut.await??;

        Ok(Json(worlds))
    }
}