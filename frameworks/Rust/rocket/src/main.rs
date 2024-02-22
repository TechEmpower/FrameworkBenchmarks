mod database;
mod models;

use std::fmt::Write;

use rand::{self, Rng};
use rocket::{launch, get, routes};
use rocket::serde::json::Json;
use rocket_db_pools::{Connection, Database};
use rocket_db_pools::sqlx;
use rocket_dyn_templates::{Template, context};

use database::HelloWorld;
use models::{Fortune, Message, World};

#[get("/plaintext")]
fn plaintext() -> &'static str {
    "Hello, World!"
}

const MESSAGE: Message = Message { message: "Hello, World!" };

#[get("/json")]
fn json() -> Json<models::Message> {
    Json(MESSAGE)
}

fn random_id() -> i32 {
    // returns a random number from 1..10,000 uniformly distributed
    let mut rng = rand::thread_rng();
    rng.gen_range(1..=10_000)
}

#[get("/db")]
async fn db(mut db: Connection<HelloWorld>) -> Json<World> {
    let number = random_id();
    let result: World = sqlx::query_as("SELECT id, randomnumber FROM World WHERE id = $1")
        .bind(number)
        .fetch_one(db.as_mut())
        .await
        .expect("error loading world");
    Json(result)
}

#[get("/queries")]
async fn queries_empty(db: Connection<HelloWorld>) -> Json<Vec<World>> {
    queries(db, 1).await
}

#[get("/queries?<q>")]
async fn queries(mut db: Connection<HelloWorld>, q: u16) -> Json<Vec<World>> {
    let q = q.clamp(1, 500);
    let mut results = Vec::with_capacity(q.into());

    for _ in 0..q {
        let query_id = random_id();
        let result: World = sqlx::query_as("SELECT * FROM World WHERE id = $1")
            .bind(query_id)
            .fetch_one(db.as_mut())
            .await
            .expect("error loading world");
        results.push(result);
    }

    Json(results)
}

#[get("/fortunes")]
async fn fortunes(mut db: Connection<HelloWorld>) -> Template {
    let mut fortunes: Vec<Fortune> = sqlx::query_as("SELECT * FROM Fortune")
        .fetch_all(db.as_mut())
        .await
        .expect("Could not load Fortunes");

    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    Template::render("fortunes", context! {
        fortunes: fortunes
    })
}

#[get("/updates")]
async fn updates_empty(db: Connection<HelloWorld>) -> Json<Vec<World>> {
    updates(db, 1).await
}

#[get("/updates?<q>")]
async fn updates(mut db: Connection<HelloWorld>, q: u16) -> Json<Vec<World>> {
    let q = q.clamp(1, 500);
    let mut results = Vec::with_capacity(q.into());

    for _ in 0..q {
        let query_id = random_id();
        let mut result: World = sqlx::query_as("SELECT * FROM World WHERE id = $1")
            .bind(query_id)
            .fetch_one(db.as_mut())
            .await
            .expect("World was not found");

        result.random_number = random_id();
        results.push(result);
    }

    let query_string = {
        let mut query = String::new();

        query.push_str("UPDATE World SET randomnumber = CASE id ");

        let mut pl = 1;

        for _ in 1..=q {
            let _ = write!(query, "when ${pl} then ${} ", pl + 1);
            pl += 2;
        }

        query.push_str("ELSE randomnumber END WHERE id IN (");

        for _ in 1..=q {
            let _ = write!(query, "${pl},");
            pl += 1;
        }

        query.pop();
        query.push(')');

        query
    };

    let mut query = sqlx::query(&query_string);

    for w in &results {
        query = query.bind(w.id).bind(w.random_number);
    }

    for w in &results {
        query = query.bind(w.id);
    }

    query.execute(db.as_mut())
        .await
        .expect("Could not update worlds");

    Json(results)
}

#[launch]
pub fn launch() -> _ {
    rocket::build()
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
        .attach(Template::fairing())
}
