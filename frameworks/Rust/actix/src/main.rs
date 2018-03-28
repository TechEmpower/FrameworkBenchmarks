extern crate actix;
extern crate actix_web;
extern crate http;
extern crate bytes;
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
use bytes::BytesMut;
use askama::Template;
use http::header;
use futures::Future;
use diesel::prelude::{Connection, PgConnection};

mod db;
mod schema;
mod models;
mod utils;
use utils::Writer;

struct State {
    db: Addr<Syn, db::DbExecutor>
}

fn world_row(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    req.state().db.send(db::RandomWorld)
        .from_err()
        .and_then(|res| {
            match res {
                Ok(row) => {
                    let mut body = BytesMut::with_capacity(31);
                    serde_json::to_writer(Writer(&mut body), &row).unwrap();
                    Ok(httpcodes::HTTPOk.build()
                       .header(header::SERVER, "Actix")
                       .content_type("application/json")
                       .body(body.freeze())?)
                },
                Err(_) =>
                    Ok(httpcodes::HTTPInternalServerError.into()),
            }
        })
        .responder()
}

fn queries(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    // get queries parameter
    let q = if let Some(q) = req.query().get("q") {
        q.parse::<u16>().ok().unwrap_or(1)
    } else {
        1
    };
    let q = cmp::min(500, cmp::max(1, q));

    // run sql queries
    req.state().db.send(db::RandomWorlds(q))
        .from_err()
        .and_then(|res| {
            if let Ok(worlds) = res {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                Ok(httpcodes::HTTPOk.build()
                   .header(header::SERVER, "Actix")
                   .content_type("application/json")
                   .body(body.freeze())?)
            } else {
                Ok(httpcodes::HTTPInternalServerError.into())
            }
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

    // update worlds
    req.state().db.send(db::UpdateWorld(q))
        .from_err()
        .and_then(move |res| {
            if let Ok(worlds) = res {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                Ok(httpcodes::HTTPOk.build()
                   .header(header::SERVER, "Actix")
                   .content_type("application/json")
                   .body(body)?)
            } else {
                Ok(httpcodes::HTTPInternalServerError.into())
            }
        })
        .responder()
}

#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<models::Fortune>,
}

fn fortune(req: HttpRequest<State>) -> Box<Future<Item=HttpResponse, Error=Error>> {
    req.state().db.send(db::TellFortune)
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
                Err(_) => {
                    Ok(httpcodes::HTTPInternalServerError.into())
                }
            }
        })
        .responder()
}

fn main() {
    let sys = System::new("techempower");
    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@TFB-database/hello_world";

    // Avoid triggering "FATAL: the database system is starting up" error from postgres.
    {
        if PgConnection::establish(db_url).is_err() {
            std::thread::sleep(std::time::Duration::from_secs(5));
        }
    }

    // Start db executor actors
    let addr = SyncArbiter::start(
        num_cpus::get() * 4, move || db::DbExecutor::new(db_url));

    // start http server
    HttpServer::new(
        move || Application::with_state(State{db: addr.clone()})
            .resource("/db", |r| r.route().a(world_row))
            .resource("/fortune", |r| r.route().a(fortune))
            .resource("/queries", |r| r.route().a(queries))
            .resource("/updates", |r| r.route().a(updates)))
        .backlog(8192)
        .bind("0.0.0.0:8080").unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8080");
    let _ = sys.run();
}
