mod common;
mod sqlx;

use std::sync::Arc;

use ::sqlx::PgPool;
use dotenv::dotenv;
use mimalloc::MiMalloc;
use quick_cache::sync::Cache;
use rama::{
    Context,
    http::{
        StatusCode,
        service::web::{
            Router,
            extract::Query,
            response::{Html, IntoResponse},
        },
    },
};
use rand::{SeedableRng, rng, rngs::SmallRng};
use sqlx::models::World;
use yarte::Template;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[cfg(not(feature = "simd-json"))]
use rama::http::service::web::response::Json;
#[cfg(feature = "simd-json")]
use rama::http::service::web::response::Json;

mod server;

use common::{
    get_env, random_id, random_ids,
    utils::{Params, parse_params},
};
use sqlx::database::create_pool;
use sqlx::models::Fortune;

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

async fn db(ctx: Context<AppState>) -> impl IntoResponse {
    let db = ctx.state().db.clone();

    let id = random_id(&mut rng());
    let world: World = ::sqlx::query_as(common::SELECT_WORLD_BY_ID)
        .bind(id)
        .fetch_one(&mut *db.acquire().await.unwrap())
        .await
        .expect("error loading world");

    (StatusCode::OK, Json(world))
}

async fn queries(
    Query(params): Query<Params>,
    ctx: Context<AppState>,
) -> impl IntoResponse {
    let db = ctx.state().db.clone();

    let mut rng = SmallRng::from_rng(&mut rng());
    let count = parse_params(params);
    let mut worlds: Vec<World> = Vec::with_capacity(count);

    for id in random_ids(&mut rng, count) {
        let world: World = ::sqlx::query_as(common::SELECT_WORLD_BY_ID)
            .bind(id)
            .fetch_one(&mut *db.acquire().await.unwrap())
            .await
            .expect("error loading world");
        worlds.push(world);
    }

    (StatusCode::OK, Json(worlds))
}

async fn fortunes(ctx: Context<AppState>) -> impl IntoResponse {
    let db = ctx.state().db.clone();

    let mut fortunes: Vec<Fortune> = ::sqlx::query_as(common::SELECT_ALL_FORTUNES)
        .fetch_all(&mut *db.acquire().await.unwrap())
        .await
        .expect("error loading Fortunes");

    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    Html(
        FortunesTemplate {
            fortunes: &fortunes,
        }
        .call()
        .expect("error rendering template"),
    )
}

async fn cache(
    Query(params): Query<Params>,
    ctx: Context<AppState>,
) -> impl IntoResponse {
    let count = parse_params(params);
    let mut rng = SmallRng::from_rng(&mut rng());
    let mut worlds: Vec<Option<World>> = Vec::with_capacity(count);

    for id in random_ids(&mut rng, count) {
        worlds.push(ctx.state().cache.get(&id));
    }

    (StatusCode::OK, Json(worlds))
}

/// Pre-load the cache with all worlds.
async fn preload_cache(AppState { db, cache }: &AppState) {
    let worlds: Vec<World> = ::sqlx::query_as(common::SELECT_ALL_CACHED_WORLDS)
        .fetch_all(&mut *db.acquire().await.unwrap())
        .await
        .expect("error loading worlds");

    for world in worlds {
        cache.insert(world.id, world);
    }
}

/// Application state
#[derive(Clone)]
struct AppState {
    db: PgPool,
    cache: Arc<Cache<i32, World>>,
}

#[tokio::main]
async fn main() {
    dotenv().ok();

    let database_url: String = get_env("POSTGRES_URL");
    let max_pool_size: u32 = get_env("POSTGRES_MAX_POOL_SIZE");
    let min_pool_size: u32 = get_env("POSTGRES_MIN_POOL_SIZE");

    let state = AppState {
        db: create_pool(database_url, max_pool_size, min_pool_size).await,
        cache: Arc::new(Cache::new(10_000)),
    };

    // Prime the cache with CachedWorld objects
    preload_cache(&state).await;

    let app = Router::new()
        .get("/fortunes", fortunes)
        .get("/db", db)
        .get("/queries", queries)
        .get("/cached-queries", cache);

    server::serve(state, app, Some(8000)).await;
}
