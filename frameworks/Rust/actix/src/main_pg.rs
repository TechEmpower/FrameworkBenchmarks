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
use std::cmp;

mod db_pg;
mod models;
mod utils;
use db_pg::{PgConnection, RandomWorld, RandomWorlds, TellFortune, UpdateWorld};
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
    let q = req
        .query()
        .get("q")
        .map(|q| cmp::min(500, cmp::max(1, q.parse::<u16>().ok().unwrap_or(1))))
        .unwrap_or(1);

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
    let addr = SyncArbiter::start(num_cpus::get() * 3, move || {
        db_pg::PgConnection::new(db_url)
    });

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
