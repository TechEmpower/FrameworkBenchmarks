#![feature(proc_macro_hygiene, decl_macro)]

extern crate rand;
#[macro_use]
extern crate rocket;
extern crate rocket_contrib;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate serde_derive;

use diesel::prelude::*;
use diesel::result::Error;
use rand::Rng;
use rocket_contrib::json::Json;
use rocket_contrib::templates::Template;

mod db;
mod models;
mod schema;

fn random_number() -> i32 {
    rand::thread_rng().gen_range(1, 10_001)
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
        let result = world
            .filter(id.eq(random_number()))
            .first::<models::World>(&*conn)
            .expect("error loading world");
        results.push(result);
    }

    Json(results)
}

#[get("/fortunes")]
fn fortunes(conn: db::DbConn) -> Template {
    use schema::fortune::dsl::*;

    let mut context = fortune
        .load::<models::Fortune>(&*conn)
        .expect("error loading fortunes");

    context.push(models::Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    context.sort_by(|a, b| a.message.cmp(&b.message));

    Template::render("fortunes", &context)
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
        let mut result = world
            .filter(id.eq(random_number()))
            .first::<models::World>(&*conn)
            .expect("error loading world");
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
    rocket::ignite()
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
        .attach(Template::fairing())
        .launch();
}
