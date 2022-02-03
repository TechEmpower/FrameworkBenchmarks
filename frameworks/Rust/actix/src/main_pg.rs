#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use actix::prelude::*;
use actix_http::error::ErrorInternalServerError;
use actix_http::{HttpService, KeepAlive};
use actix_service::map_config;
use actix_web::dev::{AppConfig, Body, Server};
use actix_web::http::{header::CONTENT_TYPE, header::SERVER, HeaderValue, StatusCode};
use actix_web::{web, App, Error, HttpRequest, HttpResponse};
use bytes::{Bytes, BytesMut};
use yarte::ywrite_html;

mod db_pg;
mod models;
mod utils;
use crate::db_pg::{PgConnection, RandomWorld, RandomWorlds, TellFortune, UpdateWorld};
use crate::utils::Writer;

async fn world_row(db: web::Data<Addr<PgConnection>>) -> Result<HttpResponse, Error> {
    let res = db
        .send(RandomWorld)
        .await
        .map_err(ErrorInternalServerError)?;
    match res {
        Ok(body) => {
            let mut res = HttpResponse::with_body(StatusCode::OK, Body::Bytes(body));
            res.headers_mut()
                .insert(SERVER, HeaderValue::from_static("Actix"));
            res.headers_mut()
                .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
            Ok(res)
        }
        Err(_) => Ok(HttpResponse::InternalServerError().into()),
    }
}

async fn queries(
    req: HttpRequest,
    db: web::Data<Addr<PgConnection>>,
) -> Result<HttpResponse, Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // run sql queries
    let res = db
        .send(RandomWorlds(q))
        .await
        .map_err(ErrorInternalServerError)?;
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
}

async fn updates(
    req: HttpRequest,
    db: web::Data<Addr<PgConnection>>,
) -> Result<HttpResponse, Error> {
    // get queries parameter
    let q = utils::get_query_param(req.query_string());

    // update db
    let res = db
        .send(UpdateWorld(q))
        .await
        .map_err(ErrorInternalServerError)?;
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
}

async fn fortune(db: web::Data<Addr<PgConnection>>) -> Result<HttpResponse, Error> {
    let res = db
        .send(TellFortune)
        .await
        .map_err(ErrorInternalServerError)?;

    match res {
        Ok(fortunes) => {
            let mut body = Vec::with_capacity(2048);
            ywrite_html!(body, "{{> fortune }}");

            let mut res =
                HttpResponse::with_body(StatusCode::OK, Body::Bytes(Bytes::from(body)));
            res.headers_mut()
                .insert(SERVER, HeaderValue::from_static("Actix"));
            res.headers_mut().insert(
                CONTENT_TYPE,
                HeaderValue::from_static("text/html; charset=utf-8"),
            );
            Ok(res)
        }
        Err(_) => Ok(HttpResponse::InternalServerError().into()),
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    println!("Started http server: 127.0.0.1:8080");

    const DB_URL: &str =
        "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    // start http server
    Server::build()
        .backlog(1024)
        .bind("techempower", "0.0.0.0:8080", move || {
            HttpService::build()
                .keep_alive(KeepAlive::Os)
                .client_timeout(0)
                .h1(map_config(
                    App::new()
                        .data_factory(|| PgConnection::connect(DB_URL))
                        .service(web::resource("/db").to(world_row))
                        .service(web::resource("/queries").to(queries))
                        .service(web::resource("/fortunes").to(fortune))
                        .service(web::resource("/updates").to(updates)),
                    |_| AppConfig::default(),
                ))
                .tcp()
        })?
        .start()
        .await
}
