#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate diesel;

use actix::prelude::*;
use actix_web::dev::Body;
use actix_web::http::{header::CONTENT_TYPE, header::SERVER, HeaderValue, StatusCode};
use actix_web::{web, App, Error, HttpRequest, HttpResponse, HttpServer};
use askama::Template;
use bytes::BytesMut;
use futures::Future;

mod db_pg;
mod models;
mod utils;
use crate::db_pg::{PgConnection, RandomWorld, RandomWorlds, TellFortune, UpdateWorld};
use crate::utils::Writer;

fn world_row(
    db: web::Data<Addr<PgConnection>>,
) -> impl Future<Item = HttpResponse, Error = Error> {
    db.send(RandomWorld)
        .from_err()
        .and_then(move |res| match res {
            Ok(row) => {
                let mut body = BytesMut::with_capacity(31);
                serde_json::to_writer(Writer(&mut body), &row).unwrap();
                let mut res =
                    HttpResponse::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
                res.headers_mut()
                    .insert(SERVER, HeaderValue::from_static("Actix"));
                res.headers_mut()
                    .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
                Ok(res)
            }
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        })
}

fn queries(
    req: HttpRequest,
    db: web::Data<Addr<PgConnection>>,
) -> impl Future<Item = HttpResponse, Error = Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // run sql queries
    db.send(RandomWorlds(q)).from_err().and_then(move |res| {
        if let Ok(worlds) = res {
            let mut body = BytesMut::with_capacity(35 * worlds.len());
            serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
            let mut res =
                HttpResponse::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
            res.headers_mut()
                .insert(SERVER, HeaderValue::from_static("Actix"));
            res.headers_mut()
                .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
            Ok(res)
        } else {
            Ok(HttpResponse::InternalServerError().into())
        }
    })
}

fn updates(
    req: HttpRequest,
    db: web::Data<Addr<PgConnection>>,
) -> impl Future<Item = HttpResponse, Error = Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // update db
    db.send(UpdateWorld(q)).from_err().and_then(move |res| {
        if let Ok(worlds) = res {
            let mut body = BytesMut::with_capacity(35 * worlds.len());
            serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
            let mut res =
                HttpResponse::with_body(StatusCode::OK, Body::Bytes(body.freeze()));
            res.headers_mut()
                .insert(SERVER, HeaderValue::from_static("Actix"));
            res.headers_mut()
                .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
            Ok(res)
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
    db: web::Data<Addr<PgConnection>>,
) -> impl Future<Item = HttpResponse, Error = Error> {
    db.send(TellFortune)
        .from_err()
        .and_then(move |res| match res {
            Ok(rows) => {
                let tmpl = FortuneTemplate { items: &rows };
                let body = tmpl.render().unwrap();

                let mut res =
                    HttpResponse::with_body(StatusCode::OK, Body::Bytes(body.into()));
                res.headers_mut()
                    .insert(SERVER, HeaderValue::from_static("Actix"));
                res.headers_mut().insert(
                    CONTENT_TYPE,
                    HeaderValue::from_static("text/html; charset=utf-8"),
                );
                Ok(res)
            }
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        })
}

fn main() -> std::io::Result<()> {
    let sys = actix_rt::System::new("techempower");
    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // start http server
    HttpServer::new(move || {
        let addr = PgConnection::connect(db_url);

        App::new()
            .data(addr)
            .service(web::resource("/db").to_async(world_row))
            .service(web::resource("/queries").to_async(queries))
            .service(web::resource("/fortune").to_async(fortune))
            .service(web::resource("/updates").to_async(updates))
    })
    .backlog(1024)
    .bind("0.0.0.0:8080")?
    .start();

    println!("Started http server: 127.0.0.1:8080");
    sys.run()
}
