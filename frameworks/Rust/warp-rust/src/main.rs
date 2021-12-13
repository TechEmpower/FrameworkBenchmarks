use futures::stream::futures_unordered::FuturesUnordered;
use futures::{FutureExt, StreamExt};
use rand::distributions::{Distribution, Uniform};
use rand::rngs::SmallRng;
use rand::SeedableRng;
use serde::Serialize;
use sqlx::postgres::PgPool;
use sqlx::FromRow;
use std::cell::RefCell;
use warp::http::header;
use warp::{Filter, Rejection, Reply};
use yarte::Template;

const DATABASE_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

// SmallRng is not a CSPRNG. It's used specifically to match performance of
// benchmarks in other programming languages where the default RNG algorithm
// is not a CSPRNG. Most Rust programs will likely want to use
// rand::thread_rng instead which is more convenient to use and safer.
thread_local!(static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::from_entropy()));

fn with_rng<T>(f: impl FnOnce(&mut SmallRng) -> T) -> T {
    RNG.with(|rng| f(&mut rng.borrow_mut()))
}

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

fn json() -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    warp::path!("json").map(|| {
        warp::reply::json(&Message {
            message: "Hello, world!",
        })
    })
}

fn plaintext() -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    warp::path!("plaintext").map(|| "Hello, World!")
}

#[derive(Serialize, FromRow)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

impl World {
    async fn get_by_id(pool: &PgPool, id: i32) -> Self {
        sqlx::query_as("SELECT id, randomnumber FROM world WHERE id=$1")
            .bind(id)
            .fetch_one(pool)
            .await
            .unwrap()
    }
}

fn db(pool: &'static PgPool) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let between = Uniform::from(1..=10_000);
    warp::path!("db").and_then(move || async move {
        let id = with_rng(|rng| between.sample(rng));
        Ok::<_, Rejection>(warp::reply::json(&World::get_by_id(pool, id).await))
    })
}

fn queries(pool: &'static PgPool) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let between = Uniform::from(1..=10_000);
    let clamped = warp::path!(u32).map(|queries: u32| queries.clamp(1, 500));
    warp::path!("queries" / ..)
        .and(clamped.or(warp::any().map(|| 1)).unify())
        .and_then(move |queries| {
            with_rng(|rng| {
                (0..queries)
                    .map(|_| World::get_by_id(pool, between.sample(rng)))
                    .collect::<FuturesUnordered<_>>()
                    .collect::<Vec<_>>()
                    .map(|worlds| Ok::<_, Rejection>(warp::reply::json(&worlds)))
            })
        })
}

#[derive(Template)]
#[template(path = "fortune")]
struct FortunesYarteTemplate {
    fortunes: Vec<Fortune>,
}

#[derive(FromRow)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

fn fortune(pool: &'static PgPool) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    warp::path!("fortunes").and_then(move || async move {
        let mut fortunes = sqlx::query_as("SELECT id, message FROM fortune")
            .fetch_all(pool)
            .await
            .unwrap();
        fortunes.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".into(),
        });
        fortunes.sort_by(|a, b| a.message.cmp(&b.message));
        Ok::<_, Rejection>(warp::reply::html(
            FortunesYarteTemplate { fortunes }.call().unwrap(),
        ))
    })
}

fn update(pool: &'static PgPool) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let between = Uniform::from(1..=10_000);
    let clamped = warp::path!(u32).map(|queries: u32| queries.clamp(1, 500));
    warp::path!("updates" / ..)
        .and(clamped.or(warp::any().map(|| 1)).unify())
        .and_then(move |queries| async move {
            let mut worlds = with_rng(|rng| {
                (0..queries)
                    .map(|_| World::get_by_id(pool, between.sample(rng)))
                    .collect::<FuturesUnordered<_>>()
                    .collect::<Vec<_>>()
            })
            .await;
            with_rng(|rng| {
                for world in &mut worlds {
                    world.randomnumber = between.sample(rng);
                }
            });
            let mut transaction = pool.begin().await.unwrap();
            for world in &worlds {
                sqlx::query("UPDATE world SET randomnumber = $1 WHERE id = $2")
                    .bind(world.randomnumber)
                    .bind(world.id)
                    .execute(&mut transaction)
                    .await
                    .unwrap();
            }
            transaction.commit().await.unwrap();
            Ok::<_, Rejection>(warp::reply::json(&worlds))
        })
}

fn routes(pool: &'static PgPool) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    json()
        .or(plaintext())
        .or(db(pool))
        .or(queries(pool))
        .or(fortune(pool))
        .or(update(pool))
        .map(|reply| warp::reply::with_header(reply, header::SERVER, "warp"))
}

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let pool = Box::leak(Box::new(PgPool::connect(DATABASE_URL).await?));
    warp::serve(routes(pool)).run(([0, 0, 0, 0], 8080)).await;
    Ok(())
}
