mod db;

use crate::db::Database;
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

fn routes(
    database: &'static Database,
) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    json()
        .or(plaintext())
        .or(db(database))
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
