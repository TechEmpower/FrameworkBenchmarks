extern crate actix;
extern crate actix_web;
extern crate http;
extern crate rand;
extern crate num_cpus;
extern crate futures;
extern crate postgres;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate askama;

use std::{io, cmp};
use actix_web::*;
use actix::prelude::*;
use askama::Template;
use http::header;
use postgres::{Connection, TlsMode};
use rand::{thread_rng, Rng, ThreadRng};
use rand::distributions::{Range, IndependentSample};
use futures::{Future, Stream, stream};

#[derive(Serialize)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[derive(Serialize)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}


struct State {
    db: SyncAddress<PgConnection>
}

fn world_row(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    req.state().db.call_fut(RandomWorld)
        .from_err()
        .and_then(|res| {
            match res {
                Ok(row) => {
                    let body = serde_json::to_string(&row).unwrap();
                    Ok(httpcodes::HTTPOk.build()
                       .header(header::SERVER, "Actix")
                       .content_type("application/json")
                       .body(body)?)
                },
                Err(_) => Ok(httpcodes::HTTPInternalServerError.into()),
            }
        })
        .responder()
}

fn queries(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    // get queries parameter
    let q = if let Some(q) = req.query().get("q") {
        q.parse::<usize>().ok().unwrap_or(1)
    } else {
        1
    };
    let q = cmp::min(500, cmp::max(1, q));

    // run sql queries
    let stream = (0..q).map(|_| req.state().db.call_fut(RandomWorld));
    stream::futures_unordered(stream)
        .from_err()
        .fold(Vec::with_capacity(q), |mut list, val|
              match val {
                  Ok(val) => {
                      list.push(val);
                      Ok(list)
                  },
                  Err(e) => Err(e)
              }
        )
        .and_then(|res| {
            let body = serde_json::to_string(&res).unwrap();
            Ok(httpcodes::HTTPOk.build()
               .header(header::SERVER, "Actix")
               .content_type("application/json")
               .content_encoding(headers::ContentEncoding::Identity)
               .body(body)?)
        })
        .responder()
}

fn updates(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    // get queries parameter
    let q = if let Some(q) = req.query().get("q") {
        q.parse::<usize>().ok().unwrap_or(1)
    } else {
        1
    };
    let q = cmp::min(500, cmp::max(1, q));

    // get rundom world objects
    let mut stream = Vec::with_capacity(q);
    for _ in 0..q {
        stream.push(req.state().db.call_fut(RandomWorld));
    }
    stream::futures_unordered(stream)
        .from_err()
        .fold(Vec::with_capacity(q), |mut list, val|
              match val {
                  Ok(val) => {
                      list.push(val);
                      Ok(list)
                  },
                  Err(e) => Err(e)
              }
        )
        .and_then(move |mut worlds| {
            // update worlds
            let mut rng = rand::thread_rng();
            let between = Range::new(1, 10_000);
            for world in &mut worlds {
                world.randomnumber = between.ind_sample(&mut rng);
            }
            let body = serde_json::to_string(&worlds).unwrap();

            // persist to db
            req.state().db.call_fut(UpdateWorld(worlds))
                .from_err()
                .and_then(move |res| {
                    if res.is_ok() {
                        Ok(httpcodes::HTTPOk.build()
                           .header(header::SERVER, "Actix")
                           .content_type("application/json")
                           .content_encoding(headers::ContentEncoding::Identity)
                           .body(body)?)
                    } else {
                        Ok(httpcodes::HTTPInternalServerError.into())
                    }
                })
        })
        .responder()
}

#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<Fortune>,
}

fn fortune(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    req.state().db.call_fut(TellFortune)
        .from_err()
        .and_then(|res| {
            match res {
                Ok(rows) => {
                    let tmpl = FortuneTemplate { items: &rows };
                    let res = tmpl.render().unwrap();

                    Ok(httpcodes::HTTPOk.build()
                       .header(header::SERVER, "Actix")
                       .content_type("text/html; charset=utf-8")
                       .content_encoding(headers::ContentEncoding::Identity)
                       .body(res)?)
                },
                Err(_) => Ok(httpcodes::HTTPInternalServerError.into())
            }
        })
        .responder()
}

/// Postgres interface
struct PgConnection {
    conn: Connection,
    rng: ThreadRng,
}

//unsafe impl Send for PgConnection {}
//unsafe impl Sync for PgConnection {}

impl Actor for PgConnection {
    type Context = SyncContext<Self>;
}

impl PgConnection {
    pub fn new(db_url: &str) -> PgConnection {
        let conn = Connection::connect(db_url, TlsMode::None)
            .expect(&format!("Error connecting to {}", db_url));
        PgConnection{
            conn: conn,
            rng: thread_rng(),
        }
    }
}

unsafe impl Send for PgConnection {}


pub struct RandomWorld;

impl ResponseType for RandomWorld {
    type Item = World;
    type Error = io::Error;
}

impl Handler<RandomWorld> for PgConnection {
    type Result = MessageResult<RandomWorld>;

    fn handle(&mut self, _: RandomWorld, _: &mut Self::Context) -> Self::Result {
        let random_world = self.conn.prepare_cached(
            "SELECT id, randomnumber FROM World WHERE id=$1").unwrap();

        let random_id = self.rng.gen_range::<i32>(1, 10_000);
        for row in &random_world.query(&[&random_id]).unwrap() {
            return Ok(World {id: row.get(0), randomnumber: row.get(1)})
        }

        Err(io::Error::new(io::ErrorKind::Other, format!("Database error")))
    }
}

pub struct UpdateWorld(pub Vec<World>);

impl ResponseType for UpdateWorld {
    type Item = ();
    type Error = ();
}

impl Handler<UpdateWorld> for PgConnection {
    type Result = ();

    fn handle(&mut self, msg: UpdateWorld, _: &mut Self::Context) {
        let update_world = self.conn.prepare_cached(
            "UPDATE World SET randomnumber=$1 WHERE id=$2").unwrap();

        for world in &msg.0 {
            let _ = update_world.execute(&[&world.randomnumber, &world.id]);
        }
    }
}

pub struct TellFortune;

impl ResponseType for TellFortune {
    type Item = Vec<Fortune>;
    type Error = io::Error;
}

impl Handler<TellFortune> for PgConnection {
    type Result = io::Result<Vec<Fortune>>;

    fn handle(&mut self, _: TellFortune, _: &mut Self::Context) -> Self::Result {
        let fortune = self.conn.prepare_cached("SELECT id, message FROM Fortune").unwrap();

        let mut items = Vec::with_capacity(13);
        items.push(
            Fortune{id: 0,
                    message: "Additional fortune added at request time.".to_string()});

        for row in &fortune.query(&[])? {
            items.push(Fortune{id: row.get(0), message: row.get(1)});
        }
        items.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(items)
    }
}


fn main() {
    let sys = System::new("techempower");
    let dbhost = match option_env!("DBHOST") {
        Some(it) => it,
        _ => "127.0.0.1"
    };
    let db_url = format!(
        "postgres://benchmarkdbuser:benchmarkdbpass@{}/hello_world", dbhost);

    // Start db executor actors
    let addr = SyncArbiter::start(
        num_cpus::get() * 3, move || PgConnection::new(&db_url));

    // start http server
    HttpServer::new(
        move || Application::with_state(State{db: addr.clone()})
            .resource("/db", |r| r.route().a(world_row))
            .resource("/queries", |r| r.route().a(queries))
            .resource("/fortune", |r| r.route().a(fortune))
            .resource("/updates", |r| r.route().a(updates)))
        .backlog(8192)
        .bind("0.0.0.0:8080").unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
