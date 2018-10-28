extern crate actix;
extern crate actix_web;
extern crate bytes;
extern crate futures;
extern crate num_cpus;
extern crate rand;
extern crate serde;
extern crate serde_json;
extern crate tokio_postgres;
extern crate url;
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

mod db_pg;
mod models;
mod utils;
use db_pg::{PgConnection, RandomWorld, RandomWorlds, TellFortune, UpdateWorld};
use utils::Writer;

struct State {
    db: Addr<PgConnection>,
}

fn world_row(req: &HttpRequest<State>) -> FutureResponse<HttpResponse> {
    let mut resp = HttpResponse::build_from(req);
    req.state()
        .db
        .send(RandomWorld)
        .from_err()
        .and_then(move |res| match res {
            Ok(row) => {
                let mut body = BytesMut::with_capacity(31);
                serde_json::to_writer(Writer(&mut body), &row).unwrap();
                Ok(resp
                    .header(http::header::SERVER, "Actix")
                    .content_type("application/json")
                    .body(body))
            }
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        }).responder()
}

fn queries(req: &HttpRequest<State>) -> FutureResponse<HttpResponse> {
    // get queries parameter
    let q = utils::get_query_param(req.uri());

    // run sql queries
    let mut resp = HttpResponse::build_from(req);
    req.clone()
        .state()
        .db
        .send(RandomWorlds(q))
        .from_err()
        .and_then(move |res| {
            if let Ok(worlds) = res {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                Ok(resp
                    .header(http::header::SERVER, "Actix")
                    .content_type("application/json")
                    .body(body))
            } else {
                Ok(HttpResponse::InternalServerError().into())
            }
        }).responder()
}

fn updates(req: &HttpRequest<State>) -> FutureResponse<HttpResponse> {
    // get queries parameter
    let q = utils::get_query_param(req.uri());

    // update db
    let mut resp = HttpResponse::build_from(req);
    req.state()
        .db
        .send(UpdateWorld(q))
        .from_err()
        .and_then(move |res| {
            if let Ok(worlds) = res {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                Ok(resp
                    .header(http::header::SERVER, "Actix")
                    .content_type("application/json")
                    .body(body))
            } else {
                Ok(HttpResponse::InternalServerError().into())
            }
        }).responder()
}

#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<models::Fortune>,
}

fn fortune(req: &HttpRequest<State>) -> FutureResponse<HttpResponse> {
    let mut resp = HttpResponse::build_from(req);
    req.state()
        .db
        .send(TellFortune)
        .from_err()
        .and_then(move |res| match res {
            Ok(rows) => {
                let tmpl = FortuneTemplate { items: &rows };
                let res = tmpl.render().unwrap();

                Ok(resp
                    .header(http::header::SERVER, "Actix")
                    .content_type("text/html; charset=utf-8")
                    .body(res))
            }
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        }).responder()
}

fn main() {
    let sys = System::new("techempower");
    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // start http server
    server::new(move || {
        let addr = PgConnection::connect(db_url);

        App::with_state(State { db: addr })
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
