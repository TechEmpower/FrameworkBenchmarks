mod db;

use crate::db::Database;
use futures::stream::futures_unordered::FuturesUnordered;
use futures::{FutureExt, StreamExt};
use rand::distributions::{Distribution, Uniform};
use serde::Serialize;
use warp::http::header;
use warp::{Filter, Rejection, Reply};

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

fn db(database: &'static Database) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let between = Uniform::from(1..=10_000);
    warp::path!("db").and_then(move || {
        async move {
            let id = between.sample(&mut rand::thread_rng());
            let world = database.get_world_by_id(id).await;
            Ok::<_, Rejection>(warp::reply::json(&world))
        }
    })
}

fn queries(
    database: &'static Database,
) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let between = Uniform::from(1..=10_000);
    let clamped = warp::path!(u32).map(|queries: u32| queries.max(1).min(500));
    warp::path!("queries" / ..)
        .and(clamped.or(warp::any().map(|| 1)).unify())
        .and_then(move |queries| {
            let mut rng = rand::thread_rng();
            (0..queries)
                .map(|_| database.get_world_by_id(between.sample(&mut rng)))
                .collect::<FuturesUnordered<_>>()
                .collect::<Vec<_>>()
                .map(|worlds| Ok::<_, Rejection>(warp::reply::json(&worlds)))
        })
}

fn routes(
    database: &'static Database,
) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    json()
        .or(plaintext())
        .or(db(database))
        .or(queries(database))
        .map(|reply| warp::reply::with_header(reply, header::SERVER, "warp"))
}

#[tokio::main]
async fn main() -> Result<(), tokio_postgres::Error> {
    let database = Box::leak(Box::new(Database::connect().await?));
    warp::serve(routes(database))
        .run(([0, 0, 0, 0], 8080))
        .await;
    Ok(())
}
