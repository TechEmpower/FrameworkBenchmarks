extern crate futures;
extern crate futures_cpupool;
extern crate num_cpus;
extern crate postgres;
extern crate r2d2;
extern crate r2d2_postgres;
extern crate rand;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate tokio_minihttp;
extern crate tokio_proto;
extern crate tokio_service;
extern crate url;

use std::cmp;
use std::io;

use futures::future::{self, Either};
use futures::stream;
use futures::{Future, Stream};
use futures_cpupool::{CpuFuture, CpuPool};
use r2d2_postgres::{PostgresConnectionManager, TlsMode};
use rand::Rng;
use tokio_minihttp::{Http, Request, Response};
use tokio_proto::TcpServer;
use tokio_service::Service;
use url::Url;

struct Techempower {
    thread_pool: CpuPool,
    db_pool: r2d2::Pool<r2d2_postgres::PostgresConnectionManager>,
}

#[allow(bad_style)]
#[derive(Serialize)]
struct WorldRow {
    id: i32,
    randomNumber: i32,
}

#[derive(Serialize)]
struct Message<'a> {
    message: &'a str,
}

impl Service for Techempower {
    type Request = Request;
    type Response = Response;
    type Error = std::io::Error;
    type Future =
        Either<future::Ok<Response, io::Error>, Box<Future<Item = Response, Error = io::Error>>>;

    fn call(&self, req: Request) -> Self::Future {
        // Bare-bones router
        match req.path() {
            "/plaintext" => Either::A(future::ok(self.plaintext())),
            "/json" => Either::A(future::ok(self.json())),
            "/db" => Either::B(self.db()),
            p if p.starts_with("/queries") => Either::B(self.queries(&req)),
            _ => {
                let mut resp = Response::new();
                resp.status_code(404, "Not Found");
                Either::A(future::ok(resp))
            }
        }
    }
}

impl Techempower {
    fn plaintext(&self) -> Response {
        let mut resp = Response::new();
        resp.header("Content-Type", "text/plain")
            .body("Hello, World!");
        return resp;
    }

    fn json(&self) -> Response {
        let json = Message {
            message: "Hello, World!",
        };

        let mut resp = Response::new();
        resp.header("Content-Type", "application/json")
            .body(&serde_json::to_string(&json).unwrap());
        return resp;
    }

    fn db(&self) -> Box<Future<Item = Response, Error = io::Error>> {
        let msg = self.random_world_row();
        Box::new(msg.map(|msg| {
            let mut resp = Response::new();
            resp.header("Content-Type", "application/json")
                .body(&serde_json::to_string(&msg).unwrap());
            return resp;
        }))
    }

    fn queries(&self, req: &Request) -> Box<Future<Item = Response, Error = io::Error>> {
        let url = format!("http://localhost{}", req.path());
        let url = Url::parse(&url).unwrap();
        let queries = url
            .query_pairs()
            .find(|pair| pair.0 == "queries")
            .and_then(|(_, value)| value.parse::<u32>().ok())
            .unwrap_or(1);
        let queries = cmp::max(1, queries);
        let queries = cmp::min(500, queries);

        let stream = (0..queries).map(|_| self.random_world_row());

        Box::new(stream::futures_unordered(stream).collect().map(|list| {
            let mut json = Vec::new();
            for row in list {
                json.push(WorldRow {
                    id: row.id,
                    randomNumber: row.randomNumber,
                });
            }
            let mut resp = Response::new();
            resp.header("Content-Type", "application/json")
                .body(&serde_json::to_string(&json).unwrap());
            return resp;
        }))
    }

    fn random_world_row(&self) -> CpuFuture<WorldRow, io::Error> {
        let random_id = rand::thread_rng().gen_range(1, 10_001);
        let db = self.db_pool.clone();
        self.thread_pool.spawn_fn(move || {
            let conn = db
                .get()
                .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("timeout: {}", e)))?;

            let stmt = conn.prepare_cached(
                "SELECT id,randomNumber \
                 FROM World WHERE id = $1",
            )?;
            let rows = stmt.query(&[&random_id])?;
            let row = rows.get(0);

            Ok(WorldRow {
                id: row.get(0),
                randomNumber: row.get(1),
            })
        })
    }
}

fn main() {
    let addr = "0.0.0.0:8080".parse().unwrap();
    let thread_pool = CpuPool::new(10);

    let dbhost = match option_env!("DBHOST") {
        Some(it) => it,
        _ => "tfb-database",
    };
    let db_url = format!(
        "postgres://benchmarkdbuser:benchmarkdbpass@{}/hello_world",
        dbhost
    );
    let db_config = r2d2::Config::default();
    let db_manager = PostgresConnectionManager::new(&db_url[..], TlsMode::None).unwrap();
    let db_pool = r2d2::Pool::new(db_config, db_manager).unwrap();

    let mut srv = TcpServer::new(Http, addr);
    srv.threads(num_cpus::get());
    srv.serve(move || {
        Ok(Techempower {
            thread_pool: thread_pool.clone(),
            db_pool: db_pool.clone(),
        })
    })
}
