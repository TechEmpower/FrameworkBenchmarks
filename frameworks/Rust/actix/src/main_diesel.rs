#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

#[macro_use]
extern crate diesel;

use actix::prelude::*;
use actix_http::error::ErrorInternalServerError;
use actix_web::{http, web, App, Error, HttpRequest, HttpResponse, HttpServer};
use askama::Template;
use bytes::BytesMut;

mod db;
mod models;
mod schema;
mod utils;

use utils::Writer;

async fn world_row(db: web::Data<Addr<db::DbExecutor>>) -> Result<HttpResponse, Error> {
    let res = db
        .send(db::RandomWorld)
        .await
        .map_err(|e| ErrorInternalServerError(e))?;

    match res {
        Ok(row) => {
            let mut body = BytesMut::with_capacity(33);
            serde_json::to_writer(Writer(&mut body), &row).unwrap();
            Ok(HttpResponse::Ok()
                .header(http::header::SERVER, "Actix")
                .header(http::header::CONTENT_TYPE, "application/json")
                .body(body))
        }
        Err(_) => Ok(HttpResponse::InternalServerError().into()),
    }
}

async fn queries(
    req: HttpRequest,
    db: web::Data<Addr<db::DbExecutor>>,
) -> Result<HttpResponse, Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // run sql queries
    let res = db
        .send(db::RandomWorlds(q))
        .await
        .map_err(|e| ErrorInternalServerError(e))?;
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
}

async fn updates(
    req: HttpRequest,
    db: web::Data<Addr<db::DbExecutor>>,
) -> Result<HttpResponse, Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // update worlds
    let res = db
        .send(db::UpdateWorld(q))
        .await
        .map_err(|e| ErrorInternalServerError(e))?;

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
}

#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<models::Fortune>,
}

async fn fortune(db: web::Data<Addr<db::DbExecutor>>) -> Result<HttpResponse, Error> {
    let res = db
        .send(db::TellFortune)
        .await
        .map_err(|e| ErrorInternalServerError(e))?;
    match res {
        Ok(rows) => {
            let tmpl = FortuneTemplate { items: &rows };
            let res = tmpl.render().unwrap();

            Ok(HttpResponse::Ok()
                .header(http::header::SERVER, "Actix")
                .header(http::header::CONTENT_TYPE, "text/html; charset=utf-8")
                .body(res))
        }
        Err(_) => Ok(HttpResponse::InternalServerError().into()),
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    println!("Starting http server: 127.0.0.1:8080");

    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // Start db executor actors
    let addr =
        SyncArbiter::start(num_cpus::get() * 3, move || db::DbExecutor::new(db_url));

    // start http server
    HttpServer::new(move || {
        App::new()
            .data(addr.clone())
            .service(web::resource("/db").to(world_row))
            .service(web::resource("/fortunes").to(fortune))
            .service(web::resource("/queries").to(queries))
            .service(web::resource("/updates").to(updates))
    })
    .bind("0.0.0.0:8080")?
    .run()
    .await
}
