extern crate actix;
extern crate actix_web;
extern crate http;
extern crate rand;
extern crate num_cpus;
extern crate futures;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate diesel;
#[macro_use] extern crate askama;

use std::cmp;
use actix_web::*;
use actix::prelude::*;
use askama::Template;
use http::header;
use rand::distributions::{Range, IndependentSample};
use futures::{Future, Stream, stream};

mod db;
mod schema;
mod models;

struct State {
    db: SyncAddress<db::DbExecutor>
}

fn json(_: HttpRequest<State>) -> Result<HttpResponse> {
    let message = models::Message {
        message: "Hello, World!"
    };
    Ok(httpcodes::HTTPOk
       .build()
       .header(header::SERVER, "Actix")
       .json(message)?)
}

fn plaintext(_: HttpRequest<State>) -> Result<HttpResponse> {
    Ok(httpcodes::HTTPOk.build()
       .header(header::SERVER, "Actix")
       .content_type("text/plain")
       .body("Hello, World!")?)
}

fn world_row(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    req.state().db.call_fut(db::RandomWorld)
        .from_err()
        .and_then(|res| {
            match res {
                Ok(row) => Ok(
                    httpcodes::HTTPOk.build()
                        .header(header::SERVER, "Actix")
                        .json(row)?),
                Err(_) =>
                    Ok(httpcodes::HTTPInternalServerError.response()),
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
    let stream = (0..q).map(|_| req.state().db.call_fut(db::RandomWorld));
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
        .and_then(|res|
            Ok(httpcodes::HTTPOk.build()
               .header(header::SERVER, "Actix")
               .content_encoding(headers::ContentEncoding::Identity)
               .json(res)?)
        )
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
        stream.push(req.state().db.call_fut(db::RandomWorld));
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
            req.state().db.call_fut(db::UpdateWorld(worlds))
                .from_err()
                .and_then(move |res| {
                    if res.is_ok() {
                        Ok(httpcodes::HTTPOk.build()
                           .header(header::SERVER, "Actix")
                           .content_type("application/json")
                           .content_encoding(headers::ContentEncoding::Identity)
                           .body(body)?)
                    } else {
                        Ok(httpcodes::HTTPInternalServerError.response())
                    }
                })
        })
        .responder()
}

#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<models::Fortune>,
}

fn fortune(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    req.state().db.call_fut(db::TellFortune)
        .from_err()
        .and_then(|res| {
            match res {
                Ok(rows) => {
                    let tmpl = FortuneTemplate { items: &rows };
                    let res = tmpl.render().unwrap();

                    Ok(httpcodes::HTTPOk.build()
                       .header(header::SERVER, "Actix")
                       .content_encoding(headers::ContentEncoding::Identity)
                       .content_type("text/html; charset=utf-8")
                       .body(res)?)
                },
                Err(_) => Ok(httpcodes::HTTPInternalServerError.response())
            }
        })
        .responder()
}

use std::env;
fn main() {
    let sys = System::new("techempower");
    env::set_var("RUST_BACKTRACE", "1");

    let dbhost = match option_env!("DBHOST") {
        Some(it) => it,
        _ => "127.0.0.1"
    };
    let db_url = format!(
        "postgres://benchmarkdbuser:benchmarkdbpass@{}/hello_world", dbhost);

    // Start db executor actors
    let addr = SyncArbiter::start(
        num_cpus::get() * 3, move || db::DbExecutor::new(&db_url));

    // start http server
    HttpServer::new(
        move || Application::with_state(State{db: addr.clone()})
            .resource("/json", |r| r.f(json))
            .resource("/plaintext", |r| r.f(plaintext))
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
