extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate num_cpus;
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
use diesel::prelude::{Connection, PgConnection};
use futures::Future;
use std::cmp;

mod db;
mod models;
mod schema;
mod utils;
use utils::Writer;

struct State {
    db: Addr<Syn, db::DbExecutor>,
}

fn world_row(req: HttpRequest<State>) -> FutureResponse<HttpResponse> {
    req.clone()
        .state()
        .db
        .send(db::RandomWorld)
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
        .send(db::RandomWorlds(q))
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
        q.parse::<usize>().ok().unwrap_or(1)
    } else {
        1
    };
    let q = cmp::min(500, cmp::max(1, q));

    // update worlds
    req.clone()
        .state()
        .db
        .send(db::UpdateWorld(q))
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
    req.clone()
        .state()
        .db
        .send(db::TellFortune)
        .from_err()
        .and_then(move |res| match res {
            Ok(rows) => {
                let tmpl = FortuneTemplate { items: &rows };
                let res = tmpl.render().unwrap();

                Ok(HttpResponse::build_from(&req)
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
        if PgConnection::establish(db_url).is_err() {
            std::thread::sleep(std::time::Duration::from_secs(5));
        }
    }

    // Start db executor actors
    let addr =
        SyncArbiter::start(num_cpus::get() * 3, move || db::DbExecutor::new(db_url));

    // start http server
    server::new(move || {
        App::with_state(State { db: addr.clone() })
            .resource("/db", |r| r.route().f(world_row))
            .resource("/fortune", |r| r.route().f(fortune))
            .resource("/queries", |r| r.route().f(queries))
            .resource("/updates", |r| r.route().f(updates))
    }).backlog(8192)
        .bind("0.0.0.0:8080")
        .unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
