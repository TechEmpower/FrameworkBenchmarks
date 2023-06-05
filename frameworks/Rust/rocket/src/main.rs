#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rocket;
extern crate dotenv;
extern crate serde_derive;

mod database;
mod models;
mod random;

use dotenv::dotenv;
use figment::Figment;
use rocket::config::{Config, LogLevel};
use rocket::response::content::RawHtml;
use rocket::serde::json::Json;
use rocket::{Build, Rocket};
use rocket_db_pools::{sqlx, Connection, Database};
use sqlx::Acquire;
use std::env;
use std::net::{IpAddr, Ipv4Addr};
use std::thread::available_parallelism;
use yarte::Template;

use database::HelloWorld;
use models::{Fortune, Message, World};
use random::random_number;

#[get("/plaintext")]
async fn plaintext() -> &'static str {
    "Hello, World!"
}

#[get("/json")]
async fn json() -> Json<models::Message> {
    let message = Message {
        message: "Hello, World!",
    };
    Json(message)
}

#[get("/db")]
async fn db(mut db: Connection<HelloWorld>) -> Json<World> {
    let number = random_number();

    let result: World = sqlx::query_as("SELECT id, randomnumber FROM World WHERE id = $1")
        .bind(number)
        .fetch_one(&mut *db)
        .await
        .ok()
        .expect("error loading world");

    Json(result)
}

#[get("/queries")]
async fn queries_empty(db: Connection<HelloWorld>) -> Json<Vec<World>> {
    queries(db, 1).await
}

#[get("/queries?<q>")]
async fn queries(mut db: Connection<HelloWorld>, q: u16) -> Json<Vec<World>> {
    let q = if q == 0 {
        1
    } else if q > 500 {
        500
    } else {
        q
    };

    let mut results = Vec::with_capacity(q as usize);

    for _ in 0..q {
        let query_id = random_number();

        let result: World = sqlx::query_as("SELECT * FROM World WHERE id = $1")
            .bind(query_id)
            .fetch_one(&mut *db)
            .await
            .ok()
            .expect("error loading world");

        results.push(result);
    }

    Json(results)
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

#[get("/fortunes")]
async fn fortunes(mut db: Connection<HelloWorld>) -> RawHtml<String> {
    let mut fortunes: Vec<Fortune> = sqlx::query_as("SELECT * FROM Fortune")
        .fetch_all(&mut *db)
        .await
        .ok()
        .expect("Could not load Fortunes");

    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    RawHtml(
        FortunesTemplate {
            fortunes: &fortunes,
        }
        .call()
        .expect("error rendering template"),
    )
}

#[get("/updates")]
async fn updates_empty(db: Connection<HelloWorld>) -> Json<Vec<World>> {
    updates(db, 1).await
}

#[get("/updates?<q>")]
async fn updates(mut db: Connection<HelloWorld>, q: u16) -> Json<Vec<World>> {
    let q = if q == 0 {
        1
    } else if q > 500 {
        500
    } else {
        q
    };

    let mut results = Vec::with_capacity(q as usize);

    for _ in 0..q {
        let query_id = random_number();
        let mut result: World = sqlx::query_as("SELECT * FROM World WHERE id = $1")
            .bind(query_id)
            .fetch_one(&mut *db)
            .await
            .ok()
            .expect("World was not found");

        result.random_number = random_number();
        results.push(result);
    }

    let mut pool = db.into_inner();
    let mut tx = pool
        .begin()
        .await
        .ok()
        .expect("could not start transaction");

    for w in &results {
        sqlx::query("UPDATE World SET randomnumber = $1 WHERE id = $2")
            .bind(w.random_number)
            .bind(w.id)
            .execute(&mut tx)
            .await
            .ok()
            .expect("Could not update World");
    }

    tx.commit().await.ok().expect("could not update worlds");

    Json(results)
}

#[launch]
pub fn launch() -> Rocket<Build> {
    if cfg!(not(test)) {
        dotenv().ok();
    }

    let config = Config {
        address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
        port: 8000,
        keep_alive: 0,
        log_level: LogLevel::Off,
        workers: available_parallelism()
            .expect("could not get parallelism")
            .get()
            * 16,
        ..Default::default()
    };

    let database_url = env::var("ROCKET_BENCHMARK_DATABASE_URL")
        .ok()
        .expect("ROCKET_BENCHMARK_DATABASE_URL environment variable was not set");

    let figment = Figment::from(config).merge((
        "databases.hello_world",
        rocket_db_pools::Config {
            url: database_url,
            min_connections: None,
            max_connections: 100,
            connect_timeout: 3,
            idle_timeout: None,
        },
    ));

    rocket::custom(figment)
        .mount(
            "/",
            routes![
                json,
                plaintext,
                db,
                queries,
                queries_empty,
                fortunes,
                updates,
                updates_empty,
            ],
        )
        .attach(HelloWorld::init())
}
