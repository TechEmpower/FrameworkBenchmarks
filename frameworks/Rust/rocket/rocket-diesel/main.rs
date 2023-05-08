use std::net::{IpAddr, Ipv4Addr};

use rand::{rngs::SmallRng, Rng, SeedableRng};

use rocket::http::Status;
use rocket::response::Redirect;
use rocket::{figment::Figment, log::LogLevel, serde::json::Json, Config};
use rocket_dyn_templates::{context, Template};

use diesel::prelude::*;

use crate::database::Db;

use crate::models::Fortune;
use crate::models::Message;
use crate::models::World;

use crate::schema::fortune;
use crate::schema::world;

#[macro_use]
extern crate diesel;
#[macro_use]
extern crate rocket;
extern crate rocket_sync_db_pools;

mod database;
mod models;
mod schema;

fn random() -> i32 {
    SmallRng::from_entropy().gen_range(1..10_000)
}

#[get("/plaintext")]
fn plaintext() -> &'static str {
    "Hello, World!"
}

#[get("/json")]
fn json() -> Json<Message> {
    let message = Message {
        message: "Hello, World!",
    };

    Json(message)
}

#[get("/db")]
async fn db(db: Db) -> Result<Json<World>, Status> {
    let world = db
        .run(move |conn| world::table.find(random()).first::<World>(conn))
        .await;

    match world {
        Ok(world) => Ok(Json(world)),
        Err(_) => Err(Status::NotFound),
    }
}

#[get("/queries")]
fn queries_empty() -> Redirect {
    Redirect::to(uri!(queries(1)))
}

#[get("/queries?<q>")]
async fn queries(db: Db, q: u16) -> Result<Json<Vec<World>>, Status> {
    let q = q.clamp(1, 500);

    let mut results = Vec::with_capacity(q.into());

    for _ in 0..q {
        let world = db
            .run(move |conn| world::table.find(random()).first::<World>(conn))
            .await;

        match world {
            Ok(world) => results.push(world),
            Err(_) => return Err(Status::NotFound),
        }
    }

    Ok(Json(results))
}

#[get("/updates")]
fn updates_empty() -> Redirect {
    Redirect::to(uri!(updates(1)))
}

#[get("/updates?<q>")]
async fn updates(db: Db, q: u16) -> Result<Json<Vec<World>>, Status> {
    let q = q.clamp(1, 500);

    let mut results = Vec::with_capacity(q.into());

    for _ in 0..q {
        let world = db
            .run(move |conn| world::table.find(random()).first::<World>(conn))
            .await;

        match world {
            Ok(world) => results.push(world),
            Err(_) => return Err(Status::NotFound),
        }
    }

    for mut world in results.clone() {
        world.randomnumber = random();

        let result = db
            .run(move |conn| {
                diesel::update(world::table.find(world.id))
                    .set(world::randomnumber.eq(world.randomnumber))
                    .execute(conn)
            })
            .await;

        if let Err(_) = result {
            return Err(Status::InternalServerError);
        }
    }

    Ok(Json(results))
}

#[get("/fortunes")]
async fn fortunes(db: Db) -> Template {
    let mut fortunes = db
        .run(|conn| fortune::table.load::<Fortune>(conn))
        .await
        .expect("Error loading fortunes from database.");

    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    Template::render("fortunes", context! { fortunes: fortunes })
}

#[launch]
fn rocket() -> _ {
    let config = Config {
        address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
        port: 8000,
        keep_alive: 0,
        log_level: LogLevel::Off,
        ..Default::default()
    };

    let figment = Figment::from(config).merge((
        "databases.hello_world",
        rocket_sync_db_pools::Config {
            pool_size: 100,
            timeout: 3,
            url: rocket::Config::figment()
                .extract_inner("benchmark_database_url")
                .expect("Expected env: ROCKET_BENCHMARK_DATABASE_URL"),
        },
    ));

    rocket::custom(figment)
        .attach(Db::fairing())
        .attach(Template::fairing())
        .mount(
            "/",
            routes![
                plaintext,
                json,
                fortunes,
                db,
                queries_empty,
                queries,
                updates_empty,
                updates,
            ],
        )
}
