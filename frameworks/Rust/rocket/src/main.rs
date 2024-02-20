mod database;
mod models;

use rand::{self, Rng};
use rocket::{launch, get, routes};
use rocket::serde::json::Json;
use rocket_db_pools::{Connection, Database};
use rocket_db_pools::sqlx::{self, Acquire};
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

// The update endpoint doesn't work, the database deadlocks because of the amount of requests
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

    let mut tx = db
        .begin()
        .await
        .expect("could not start transaction");

    for w in &results {
        sqlx::query("UPDATE World SET randomnumber = $1 WHERE id = $2")
            .bind(w.random_number)
            .bind(w.id)
            .execute(tx.as_mut())
            .await
            .expect("Could not update World");
    }

    tx.commit().await.expect("could not update worlds");

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
