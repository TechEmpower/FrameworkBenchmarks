#![feature(proc_macro_hygiene, decl_macro)]

extern crate rand;
#[macro_use]
extern crate rocket;
extern crate rocket_contrib;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate serde_derive;
extern crate lazy_static;

use diesel::prelude::*;
use diesel::result::Error;
use lazy_static::lazy_static;
use rand::seq::SliceRandom;
use rand::thread_rng;
use rocket::config::{Config, Environment, LoggingLevel};
use rocket::response::content;
use rocket_contrib::json::Json;
use std::sync::Mutex;
use yarte::Template;

mod db;
mod models;
mod schema;

struct RandomArray {
    pointer: usize,
    size: i32,
    data: Vec<i32>,
}

impl RandomArray {
    fn new(size: i32) -> Self {
        let mut data: Vec<i32> = (1..=size).collect();
        let mut rng = thread_rng();
        data.shuffle(&mut rng);

        RandomArray {
            pointer: 0,
            size,
            data,
        }
    }

    fn next(&mut self) -> i32 {
        if self.pointer >= self.size as usize {
            self.pointer = 1;
        } else {
            self.pointer += 1;
        }
        self.data[self.pointer - 1]
    }
}

lazy_static! {
    static ref RANDOM_ARRAY: Mutex<RandomArray> = Mutex::new(RandomArray::new(10000));
}
fn random_number() -> i32 {
    RANDOM_ARRAY
        .lock()
        .expect("Failed to lock RANDOM_ARRAY")
        .next()
}

#[get("/plaintext")]
fn plaintext() -> &'static str {
    "Hello, World!"
}

#[get("/json")]
fn json() -> Json<models::Message> {
    let message = models::Message {
        message: "Hello, World!",
    };
    Json(message)
}

#[get("/db")]
fn db(conn: db::DbConn) -> Json<models::World> {
    use schema::world::dsl::*;

    let result = world
        .filter(id.eq(random_number()))
        .first::<models::World>(&*conn)
        .expect("error loading world");

    Json(result)
}

#[get("/queries")]
fn queries_empty(conn: db::DbConn) -> Json<Vec<models::World>> {
    queries(conn, 1)
}

#[get("/queries?<q>")]
fn queries(conn: db::DbConn, q: u16) -> Json<Vec<models::World>> {
    use schema::world::dsl::*;

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
        let result = world
            .filter(id.eq(query_id))
            .first::<models::World>(&*conn)
            .unwrap_or_else(|_| panic!("error loading world, id={}", query_id));
        results.push(result);
    }

    Json(results)
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<models::Fortune>,
}

#[get("/fortunes")]
fn fortunes(conn: db::DbConn) -> content::Html<String> {
    use schema::fortune::dsl::*;

    let mut fortunes = fortune
        .load::<models::Fortune>(&*conn)
        .expect("error loading fortunes");

    fortunes.push(models::Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    content::Html(
        FortunesTemplate {
            fortunes: &fortunes,
        }
        .call()
        .expect("error rendering template"),
    )
}

#[get("/updates")]
fn updates_empty(conn: db::DbConn) -> Json<Vec<models::World>> {
    updates(conn, 1)
}

#[get("/updates?<q>")]
fn updates(conn: db::DbConn, q: u16) -> Json<Vec<models::World>> {
    use schema::world::dsl::*;

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
        let mut result = world
            .filter(id.eq(query_id))
            .first::<models::World>(&*conn)
            .unwrap_or_else(|_| panic!("error loading world, id={}", query_id));
        result.randomNumber = random_number();
        results.push(result);
    }

    let _ = conn.transaction::<(), Error, _>(|| {
        for w in &results {
            let _ = diesel::update(world)
                .filter(id.eq(w.id))
                .set(randomnumber.eq(w.randomNumber))
                .execute(&*conn);
        }
        Ok(())
    });

    Json(results)
}

fn main() {
    let mut config = Config::build(Environment::Production)
        .address("0.0.0.0")
        .port(8000)
        .log_level(LoggingLevel::Off)
        .workers((num_cpus::get() * 16) as u16)
        .keep_alive(0)
        .expect("failed to generate config");
    config
        .set_secret_key("dY+Rj2ybjGxKetLawKGSWi6EzESKejvENbQ3stffZg0=")
        .expect("failed to set secret");
    rocket::custom(config)
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
        .manage(db::init_pool())
        .launch();
}
