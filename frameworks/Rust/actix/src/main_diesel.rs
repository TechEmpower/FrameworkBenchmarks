#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate diesel;

use actix::prelude::*;
use actix_http::{HttpService, KeepAlive};
use actix_server::Server;
use actix_web::{http, web, App, Error, HttpRequest, HttpResponse};
use askama::Template;
use bytes::BytesMut;
use futures::Future;

mod db;
mod models;
mod schema;
mod utils;
use utils::Writer;

fn world_row(
    db: web::Data<Addr<db::DbExecutor>>,
) -> impl Future<Item = HttpResponse, Error = Error> {
    db.send(db::RandomWorld)
        .from_err()
        .and_then(move |res| match res {
            Ok(row) => {
                let mut body = BytesMut::with_capacity(31);
                serde_json::to_writer(Writer(&mut body), &row).unwrap();
                Ok(HttpResponse::Ok()
                    .header(http::header::SERVER, "Actix")
                    .header(http::header::CONTENT_TYPE, "application/json")
                    .body(body))
            }
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        })
}

fn queries(
    req: HttpRequest,
    db: web::Data<Addr<db::DbExecutor>>,
) -> impl Future<Item = HttpResponse, Error = Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // run sql queries
    db.send(db::RandomWorlds(q))
        .from_err()
        .and_then(move |res| {
            if let Ok(worlds) = res {
                let mut body = BytesMut::with_capacity(35 * worlds.len());
                serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
                Ok(HttpResponse::Ok()
                    .header(http::header::SERVER, "Actix")
                    .header(http::header::CONTENT_TYPE, "application/json")
                    .body(body))
            } else {
                Ok(HttpResponse::InternalServerError().into())
            }
        })
}

fn updates(
    req: HttpRequest,
    db: web::Data<Addr<db::DbExecutor>>,
) -> impl Future<Item = HttpResponse, Error = Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // update worlds
    db.send(db::UpdateWorld(q)).from_err().and_then(move |res| {
        if let Ok(worlds) = res {
            let mut body = BytesMut::with_capacity(35 * worlds.len());
            serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
            Ok(HttpResponse::Ok()
                .header(http::header::SERVER, "Actix")
                .header(http::header::CONTENT_TYPE, "application/json")
                .body(body))
        } else {
            Ok(HttpResponse::InternalServerError().into())
        }
    })
}

#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<models::Fortune>,
}

fn fortune(
    db: web::Data<Addr<db::DbExecutor>>,
) -> impl Future<Item = HttpResponse, Error = Error> {
    db.send(db::TellFortune)
        .from_err()
        .and_then(move |res| match res {
            Ok(rows) => {
                let tmpl = FortuneTemplate { items: &rows };
                let res = tmpl.render().unwrap();

                Ok(HttpResponse::Ok()
                    .header(http::header::SERVER, "Actix")
                    .header(http::header::CONTENT_TYPE, "text/html; charset=utf-8")
                    .body(res))
            }
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        })
}

fn main() -> std::io::Result<()> {
    let sys = actix_rt::System::new("techempower");
    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // Start db executor actors
    let addr =
        SyncArbiter::start(num_cpus::get() * 3, move || db::DbExecutor::new(db_url));

    // start http server
    Server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", move || {
            HttpService::build().keep_alive(KeepAlive::Os).h1(App::new()
                .data(addr.clone())
                .service(web::resource("/db").to_async(world_row))
                .service(web::resource("/fortune").to_async(fortune))
                .service(web::resource("/queries").to_async(queries))
                .service(web::resource("/updates").to_async(updates)))
        })?
        .start();

    println!("Started http server: 127.0.0.1:8080");
    sys.run()
}
