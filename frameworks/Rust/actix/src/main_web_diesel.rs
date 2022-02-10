#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

#[macro_use]
extern crate diesel;

use actix::prelude::*;
use actix_web::{
    error,
    http::{self, header::ContentType},
    web, App, Error, HttpRequest, HttpResponse, HttpServer,
};
use askama::Template;
use bytes::BytesMut;

mod db_diesel;
mod models;
mod schema;
mod utils;

use utils::Writer;

async fn world_row(db: web::Data<Addr<db_diesel::DbExecutor>>) -> Result<HttpResponse, Error> {
    let res = db
        .send(db_diesel::RandomWorld)
        .await
        .map_err(error::ErrorInternalServerError)?;

    match res {
        Ok(row) => {
            let mut body = BytesMut::with_capacity(33);
            serde_json::to_writer(Writer(&mut body), &row).unwrap();
            Ok(HttpResponse::Ok()
                .insert_header((http::header::SERVER, "Actix"))
                .insert_header((http::header::CONTENT_TYPE, ContentType::json()))
                .body(body))
        }
        Err(_) => Ok(HttpResponse::InternalServerError().into()),
    }
}

async fn queries(
    req: HttpRequest,
    db: web::Data<Addr<db_diesel::DbExecutor>>,
) -> Result<HttpResponse, Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // run SQL queries
    let res = db
        .send(db_diesel::RandomWorlds(q))
        .await
        .map_err(error::ErrorInternalServerError)?;
    if let Ok(worlds) = res {
        let mut body = BytesMut::with_capacity(35 * worlds.len());
        serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
        Ok(HttpResponse::Ok()
            .insert_header((http::header::SERVER, "Actix"))
            .insert_header((http::header::CONTENT_TYPE, ContentType::json()))
            .body(body))
    } else {
        Ok(HttpResponse::InternalServerError().into())
    }
}

async fn updates(
    req: HttpRequest,
    db: web::Data<Addr<db_diesel::DbExecutor>>,
) -> Result<HttpResponse, Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // update worlds
    let res = db
        .send(db_diesel::UpdateWorld(q))
        .await
        .map_err(error::ErrorInternalServerError)?;

    if let Ok(worlds) = res {
        let mut body = BytesMut::with_capacity(35 * worlds.len());
        serde_json::to_writer(Writer(&mut body), &worlds).unwrap();
        Ok(HttpResponse::Ok()
            .insert_header((http::header::SERVER, "Actix"))
            .insert_header((http::header::CONTENT_TYPE, ContentType::json()))
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

async fn fortune(db: web::Data<Addr<db_diesel::DbExecutor>>) -> Result<HttpResponse, Error> {
    let res = db
        .send(db_diesel::TellFortune)
        .await
        .map_err(error::ErrorInternalServerError)?;
    match res {
        Ok(rows) => {
            let tmpl = FortuneTemplate { items: &rows };
            let res = tmpl.render().unwrap();

            Ok(HttpResponse::Ok()
                .insert_header((http::header::SERVER, "Actix"))
                .insert_header((http::header::CONTENT_TYPE, ContentType::html()))
                .body(res))
        }
        Err(_) => Ok(HttpResponse::InternalServerError().into()),
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // start DB executor actors
    let addr = SyncArbiter::start(num_cpus::get() * 3, move || {
        db_diesel::DbExecutor::new(db_url)
    });

    println!("Starting HTTP server: 127.0.0.1:8080");

    // start HTTP server
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(addr.clone()))
            .service(web::resource("/db").to(world_row))
            .service(web::resource("/fortunes").to(fortune))
            .service(web::resource("/queries").to(queries))
            .service(web::resource("/updates").to(updates))
    })
    .bind("0.0.0.0:8080")?
    .run()
    .await
}
