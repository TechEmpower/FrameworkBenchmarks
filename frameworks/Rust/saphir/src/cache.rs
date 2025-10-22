use mongodm::prelude::*;
use cached::proc_macro::cached;
use cached::TimedCache;
use crate::models::*;
use crate::errors::BenchmarkControllerError;

#[cached(
    type = "TimedCache<i32, Option<CachedWorld>>",
    create = "{ TimedCache::with_lifespan(120) }",
    convert = r#"{ id }"#,
    result = true
)]
pub async fn find_cached_world_by_id(collection: MongoCollection<CachedWorld>, id: i32) -> Result<Option<CachedWorld>, BenchmarkControllerError> {
    Ok(collection.find_one(doc! { "_id": id }, None).await?)
}