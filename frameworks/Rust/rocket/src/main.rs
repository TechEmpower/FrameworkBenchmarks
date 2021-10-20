#![feature(proc_macro_hygiene, decl_macro)]
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rocket;
extern crate serde_derive;
extern crate dotenv;

mod models;
mod random;
mod database;
mod request;

use dotenv::dotenv;
use std::net::{IpAddr, Ipv4Addr};
use std::env;
use rocket::{Rocket, Build};
use rocket::serde::json::Json;
use rocket::response::content::RawHtml;
use rocket::config::{Config, LogLevel};
use yarte::Template;
use rocket_db_pools::{sqlx, Database, Connection};
use sqlx::Acquire;
use figment::Figment;

use models::{World, Fortune, Message};
use database::HelloWorld;
use random::random_number;
use request::RequestId;

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
async fn db(mut db: Connection<HelloWorld>, id: RequestId) -> Json<World> {
    let number = random_number(&id);

    let result : World = sqlx::query_as("SELECT id, randomnumber FROM World WHERE id = $1").bind(number)
        .fetch_one(&mut *db).await.ok().expect("error loading world");

    Json(result)
}

#[get("/queries")]
async fn queries_empty(db: Connection<HelloWorld>, id: RequestId) -> Json<Vec<World>> {
    queries(db, id,1).await
}

#[get("/queries?<q>")]
async fn queries(mut db: Connection<HelloWorld>, id: RequestId, q: u16) -> Json<Vec<World>> {
    let q = if q == 0 {
        1
    } else if q > 500 {
        500
    } else {
        q
    };

    let mut results = Vec::with_capacity(q as usize);

    for _ in 0..q {
        let query_id = random_number(&id);

        let result :World = sqlx::query_as("SELECT * FROM World WHERE id = $1").bind(query_id)
            .fetch_one(&mut *db).await.ok().expect("error loading world");

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
    let mut fortunes: Vec<Fortune> = sqlx::query_as("SELECT * FROM Fortune").fetch_all(&mut *db).await
        .ok().expect("Could not load Fortunes");

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
async fn updates_empty(db: Connection<HelloWorld>, id: RequestId) -> Json<Vec<World>> {
    updates(db, id,1).await
}

#[get("/updates?<q>")]
async fn updates(mut db: Connection<HelloWorld>, id: RequestId, q: u16) -> Json<Vec<World>> {
    let q = if q == 0 {
        1
    } else if q > 500 {
        500
    } else {
        q
    };

    let mut results = Vec::with_capacity(q as usize);

    for _ in 0..q {
        let query_id = random_number(&id);
        let mut result :World = sqlx::query_as("SELECT * FROM World WHERE id = $1").bind(query_id)
            .fetch_one(&mut *db).await.ok().expect("World was not found");

        result.random_number = random_number(&id);
        results.push(result);
    }

    let mut pool = db.into_inner();
    let mut tx = pool.begin().await.ok().expect("could not start transaction");

    for w in &results {
        sqlx::query("UPDATE World SET randomnumber = $1 WHERE id = $2")
            .bind(w.random_number).bind(w.id)
            .execute(&mut tx)
            .await.ok().expect("Could not update World");
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
        workers: num_cpus::get() * 16,
        ..Default::default()
    };

    let database_url = env::var("ROCKET_BENCHMARK_DATABASE_URL").ok()
        .expect("ROCKET_BENCHMARK_DATABASE_URL environment variable was not set");

    let figment = Figment::from(config)
        .merge(("databases.hello_world", rocket_db_pools::Config {
                url: database_url,
                min_connections: None,
                max_connections: 100,
                connect_timeout: 3,
                idle_timeout: None,
            }));

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

#[cfg(test)]
mod tests
{
    use std::env;
    use rocket::{Rocket, Build};
    use rocket::{local::blocking::Client};
    use crate::database::HelloWorld;
    use rocket::fairing::{self, AdHoc};
    use rocket_db_pools::Database;

    #[test]
    fn plaintext() {
        let client = create_client();

        let response = client.get("/plaintext").dispatch();
        assert_eq!(response.into_string().unwrap(), "Hello, World!");
    }

    #[test]
    fn json() {
        let client = create_client();

        let response = client.get("/json").dispatch();
        assert_eq!(response.into_string().unwrap(), "{\"message\":\"Hello, World!\"}");
    }

    #[test]
    fn db() {
        let client = create_client();

        let response = client.get("/db").dispatch();
        assert_eq!(response.into_string().unwrap(), "{\"id\":1,\"randomNumber\":101}");
    }

    #[test]
    fn queries_empty() {
        let client = create_client();

        let response = client.get("/queries").dispatch();
        assert_eq!(response.into_string().unwrap(), "[{\"id\":1,\"randomNumber\":101}]");
    }

    #[test]
    fn queries_non_empty() {
        let client = create_client();

        let response = client.get("/queries?q=3").dispatch();
        assert_eq!(response.into_string().unwrap(), "[{\"id\":1,\"randomNumber\":101},{\"id\":2,\"randomNumber\":102},{\"id\":3,\"randomNumber\":103}]");
    }

    #[test]
    fn fortunes() {
        let client = create_client();

        let response = client.get("/fortunes").dispatch();
        assert_eq!(response.into_string().unwrap(), "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr><tr><td>11</td><td>&lt;script&gt;alert(&quot;This should not be displayed in a browser alert box.&quot;);&lt;&#x2f;script&gt;</td></tr><tr><td>4</td><td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td></tr><tr><td>5</td><td>A computer program does what you tell it to do, not what you want it to do.</td></tr><tr><td>2</td><td>A computer scientist is someone who fixes things that aren&#x27;t broken.</td></tr><tr><td>8</td><td>A list is only as strong as its weakest link. — Donald Knuth</td></tr><tr><td>0</td><td>Additional fortune added at request time.</td></tr><tr><td>3</td><td>After enough decimal places, nobody gives a damn.</td></tr><tr><td>7</td><td>Any program that runs right is obsolete.</td></tr><tr><td>10</td><td>Computers make very fast, very accurate mistakes.</td></tr><tr><td>6</td><td>Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen</td></tr><tr><td>9</td><td>Feature: A bug with seniority.</td></tr><tr><td>1</td><td>fortune: No such file or directory</td></tr><tr><td>12</td><td>フレームワークのベンチマーク</td></tr></table></body></html>");
    }

    #[test]
    fn updates_empty() {
        let client = create_client();

        let response = client.get("/updates").dispatch();
        assert_eq!(response.into_string().unwrap(), "[{\"id\":1,\"randomNumber\":2}]");
    }

    #[test]
    fn updates_non_empty() {
        let client = create_client();

        let response = client.get("/updates?q=3").dispatch();
        assert_eq!(response.into_string().unwrap(), "[{\"id\":1,\"randomNumber\":2},{\"id\":3,\"randomNumber\":4},{\"id\":5,\"randomNumber\":6}]");
    }

    fn create_client() -> Client {
        env::set_var("ROCKET_BENCHMARK_DATABASE_URL", "sqlite::memory:");

        let rocket = create_rocket();
        let client = Client::debug(rocket).unwrap();
        client
    }

    fn create_rocket() -> Rocket<Build> {
        super::launch().attach(AdHoc::try_on_ignite("SQLx Migrations", run_migrations))
    }

    async fn run_migrations(rocket: Rocket<Build>) -> fairing::Result {
        match HelloWorld::fetch(&rocket) {
            Some(db) => match sqlx::migrate!("db/migrations").run(&**db).await {
                Ok(_) => Ok(rocket),
                Err(e) => {
                    error!("Failed to initialize HelloWorld database: {}", e);
                    Err(rocket)
                }
            }
            None => Err(rocket),
        }
    }
}


