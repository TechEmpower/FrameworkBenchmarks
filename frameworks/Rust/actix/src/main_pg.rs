extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate num_cpus;
extern crate postgres;
extern crate rand;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate askama;

use actix::prelude::*;
use actix_web::{
    http, server, App, AsyncResponder, FutureResponse, HttpRequest, HttpResponse,
};
use askama::Template;
use bytes::BytesMut;
use futures::Future;
use postgres::{Connection, TlsMode};
use rand::{thread_rng, Rng, ThreadRng};
use std::{cmp, io};

mod models;
mod utils;
use models::{Fortune, World};
use utils::Writer;

struct State {
    db: Addr<Syn, PgConnection>,
}

fn world_row(req: HttpRequest<State>) -> FutureResponse<HttpResponse> {
    req.clone()
        .state()
        .db
        .send(RandomWorld)
        .from_err()
        .and_then(move |res| match res {
            Ok(row) => {
                let mut body = BytesMut::with_capacity(31);
                serde_json::to_writer(Writer(&mut body), &row).unwrap();
                Ok(HttpResponse::build_from(&req)
                    .header(http::header::SERVER, "Actix")
                    .content_type("application/json")
                    .body(body))
            }
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        })
        .responder()
}

fn queries(req: HttpRequest<State>) -> FutureResponse<HttpResponse> {
    // get queries parameter
    let q = req
        .query()
        .get("q")
        .map(|q| cmp::min(500, cmp::max(1, q.parse::<u16>().ok().unwrap_or(1))))
        .unwrap_or(1);

    // run sql queries
    req.clone()
        .state()
        .db
        .send(RandomWorlds(q))
        .from_err()
        .and_then(move |res| {
            if let Ok(worlds) = res {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                Ok(HttpResponse::build_from(&req)
                    .header(http::header::SERVER, "Actix")
                    .content_type("application/json")
                    .body(body))
            } else {
                Ok(HttpResponse::InternalServerError().into())
            }
        })
        .responder()
}

fn updates(req: HttpRequest<State>) -> FutureResponse<HttpResponse> {
    // get queries parameter
    let q = if let Some(q) = req.query().get("q") {
        q.parse::<u16>().ok().unwrap_or(1)
    } else {
        1
    };
    let q = cmp::min(500, cmp::max(1, q));

    // update db
    req.clone()
        .state()
        .db
        .send(UpdateWorld(q))
        .from_err()
        .and_then(move |res| {
            if let Ok(worlds) = res {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                Ok(HttpResponse::build_from(&req)
                    .header(http::header::SERVER, "Actix")
                    .content_type("application/json")
                    .body(body))
            } else {
                Ok(HttpResponse::InternalServerError().into())
            }
        })
        .responder()
}

#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<models::Fortune>,
}

fn fortune(req: HttpRequest<State>) -> FutureResponse<HttpResponse> {
    req.state()
        .db
        .send(TellFortune)
        .from_err()
        .and_then(|res| match res {
            Ok(rows) => {
                let tmpl = FortuneTemplate { items: &rows };
                let res = tmpl.render().unwrap();

                Ok(HttpResponse::Ok()
                    .header(http::header::SERVER, "Actix")
                    .content_type("text/html; charset=utf-8")
                    .body(res))
            }
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        })
        .responder()
}

/// Postgres interface
struct PgConnection {
    conn: Connection,
    rng: ThreadRng,
}

impl Actor for PgConnection {
    type Context = SyncContext<Self>;
}

impl PgConnection {
    pub fn new(db_url: &str) -> PgConnection {
        let conn = Connection::connect(db_url, TlsMode::None)
            .expect(&format!("Error connecting to {}", db_url));
        PgConnection {
            conn,
            rng: thread_rng(),
        }
    }
}

unsafe impl Send for PgConnection {}

pub struct RandomWorld;

impl Message for RandomWorld {
    type Result = io::Result<World>;
}

impl Handler<RandomWorld> for PgConnection {
    type Result = io::Result<World>;

    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Self::Result {
        let random_world = self
            .conn
            .prepare_cached("SELECT id, randomnumber FROM world WHERE id=$1")
            .unwrap();

        let random_id = self.rng.gen_range::<i32>(1, 10_000);
        let rows = &random_world.query(&[&random_id]).unwrap();
        let row = rows.get(0);
        Ok(World {
            id: row.get(0),
            randomnumber: row.get(1),
        })
    }
}

pub struct RandomWorlds(pub u16);

impl Message for RandomWorlds {
    type Result = io::Result<Vec<World>>;
}

impl Handler<RandomWorlds> for PgConnection {
    type Result = io::Result<Vec<World>>;

    fn handle(&mut self, msg: RandomWorlds, _: &mut Self::Context) -> Self::Result {
        let random_world = self
            .conn
            .prepare_cached("SELECT id, randomnumber FROM world WHERE id=$1")
            .unwrap();

        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let w_id: i32 = self.rng.gen_range(1, 10_000);
            let rows = &random_world.query(&[&w_id]).unwrap();
            let row = rows.get(0);
            worlds.push(World {
                id: row.get(0),
                randomnumber: row.get(1),
            });
        }
        Ok(worlds)
    }
}

pub struct UpdateWorld(pub u16);

impl Message for UpdateWorld {
    type Result = io::Result<Vec<World>>;
}

impl Handler<UpdateWorld> for PgConnection {
    type Result = io::Result<Vec<World>>;

    fn handle(&mut self, msg: UpdateWorld, _: &mut Self::Context) -> Self::Result {
        let get_world = self
            .conn
            .prepare_cached("SELECT id FROM world WHERE id=$1")
            .unwrap();
        let mut update = String::with_capacity(120 + 6 * msg.0 as usize);
        update
            .push_str("UPDATE world SET randomnumber = temp.randomnumber FROM (VALUES ");

        let mut worlds = Vec::with_capacity(msg.0 as usize);
        for _ in 0..msg.0 {
            let random_id = self.rng.gen_range::<i32>(1, 10_000);
            let rows = &get_world.query(&[&random_id]).unwrap();
            let w = World {
                id: rows.get(0).get(0),
                randomnumber: self.rng.gen_range(1, 10_000),
            };
            update.push_str(&format!("({}, {}),", w.id, w.randomnumber));
            worlds.push(w);
        }
        worlds.sort_by_key(|w| w.id);

        update.pop();
        update.push_str(") AS temp(id, randomnumber) WHERE temp.id = world.id");
        self.conn.execute(&update, &[]).unwrap();

        Ok(worlds)
    }
}

pub struct TellFortune;

impl Message for TellFortune {
    type Result = io::Result<Vec<Fortune>>;
}

impl Handler<TellFortune> for PgConnection {
    type Result = io::Result<Vec<Fortune>>;

    fn handle(&mut self, _: TellFortune, _: &mut Self::Context) -> Self::Result {
        let fortune = self
            .conn
            .prepare_cached("SELECT id, message FROM fortune")
            .unwrap();

        let mut items = Vec::with_capacity(16);
        items.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });

        for row in &fortune.query(&[])? {
            items.push(Fortune {
                id: row.get(0),
                message: row.get(1),
            });
        }
        items.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(items)
    }
}

fn main() {
    let sys = System::new("techempower");
    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // Avoid triggering "FATAL: the database system is starting up" error from
    // postgres.
    {
        if Connection::connect(db_url, TlsMode::None).is_err() {
            std::thread::sleep(std::time::Duration::from_secs(5));
        }
    }

    // Start db executor actors
    let addr =
        SyncArbiter::start(num_cpus::get() * 4, move || PgConnection::new(db_url));

    // start http server
    server::new(move || {
        App::with_state(State { db: addr.clone() })
            .resource("/db", |r| r.route().f(world_row))
            .resource("/queries", |r| r.route().f(queries))
            .resource("/fortune", |r| r.route().f(fortune))
            .resource("/updates", |r| r.route().f(updates))
    }).backlog(8192)
        .bind("0.0.0.0:8080")
        .unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
