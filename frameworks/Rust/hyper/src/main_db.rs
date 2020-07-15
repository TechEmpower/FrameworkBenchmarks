extern crate futures;
extern crate hyper;
extern crate net2;
extern crate tokio_core;
extern crate tokio_postgres;

use std::fmt::Write;
use std::net::ToSocketAddrs;

use futures::{future, Future};
use hyper::header::{HeaderValue, CONTENT_TYPE, SERVER};
use hyper::service::service_fn;
use hyper::{Body, Response};

mod db;
mod server;

fn main() {
    //"postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
    let mut psql_config = tokio_postgres::Config::new();
    psql_config
        .user("benchmarkdbuser")
        .password("benchmarkdbpass")
        .dbname("hello_world");

    let psql_addr = ("tfb-database", 5432)
        .to_socket_addrs()
        .expect("must be able to resolve database hostname")
        .next()
        .expect("database hostname must resolve to an address");

    server::run(move |socket, http, handle| {
        let http = http.clone();
        let handle2 = handle.clone();

        let html_ct = HeaderValue::from_static("text/html; charset=utf-8");
        let server_header = HeaderValue::from_static("hyper");

        // Before handling any requests, we should grab a DB connection.
        let db_fut =
            db::connect(psql_addr, psql_config.clone(), handle.clone()).map(move |mut db_conn| {
                let html_ct = html_ct.clone();
                let server_header = server_header.clone();

                // This is the `Service` that will handle the connection.
                // `service_fn` is a helper to convert a function that
                // returns a Future<Item=Response> into a `Service`.
                let svc = service_fn(move |req| {
                    let (req, _body) = req.into_parts();
                    // For speed, reuse the allocated header map from the request,
                    // instead of allocating a new one. Because.
                    let mut headers = req.headers;
                    headers.clear();

                    headers.insert(CONTENT_TYPE, html_ct.clone());
                    headers.insert(SERVER, server_header.clone());

                    match req.uri.path() {
                        "/fortunes" => {
                            future::Either::A(db_conn.tell_fortune().map(move |fortunes| {
                                let mut buf = String::with_capacity(2048);
                                let _ = write!(&mut buf, "{}", FortunesTemplate { fortunes });
                                let mut res = Response::new(Body::from(buf));
                                *res.headers_mut() = headers;
                                res
                            }))
                        }
                        _ => {
                            let mut res = Response::new(Body::empty());
                            *res.status_mut() = hyper::StatusCode::NOT_FOUND;
                            *res.headers_mut() = headers;
                            future::Either::B(future::ok(res))
                        }
                    }
                });

                // Spawn the `serve_connection` future into the runtime.
                handle2.spawn(
                    http.serve_connection(socket, svc)
                        .map_err(|e| eprintln!("connection error: {}", e)),
                );
            });
        handle.spawn(db_fut);
    });
}

markup::define! {
    FortunesTemplate(fortunes: Vec<db::Fortune>) {
        {markup::doctype()}
        html {
            head {
                title { "Fortunes" }
            }
            body {
                table {
                    tr { th { "id" } th { "message" } }
                    @for item in {fortunes} {
                        tr {
                            td { {item.id} }
                            td { {markup::raw(v_htmlescape::escape(&item.message))} }
                        }
                    }
                }
            }
        }
    }
}
