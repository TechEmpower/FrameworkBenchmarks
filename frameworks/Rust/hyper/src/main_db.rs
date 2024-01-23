use std::fmt::Write;
use std::sync::Arc;

use hyper::header::{HeaderValue, CONTENT_TYPE, SERVER};
use hyper::service::service_fn;
use hyper::{Body, Response};
use std::net::ToSocketAddrs;

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
        let config = psql_config.clone();

        // Before handling any requests, we should grab a DB connection.
        let db_fut = async move {
            let handle = handle2.clone();
            let db_conn = db::connect(psql_addr, config, handle).await?;
            let db_conn = Arc::new(db_conn);

            let html_ct = html_ct.clone();
            let server_header = server_header.clone();

            // This is the `Service` that will handle the connection.
            // `service_fn` is a helper to convert a function that
            // returns a Future<Item=Response> into a `Service`.
            let svc = service_fn(move |req| {
                let html_ct = html_ct.clone();
                let server_header = server_header.clone();
                let db_conn = db_conn.clone();

                async move {
                    let (req, _body) = req.into_parts();
                    // For speed, reuse the allocated header map from the request,
                    // instead of allocating a new one. Because.
                    let mut headers = req.headers;
                    headers.clear();

                    headers.insert(CONTENT_TYPE, html_ct.clone());
                    headers.insert(SERVER, server_header.clone());

                    match req.uri.path() {
                        "/fortunes" => {
                            let fortunes = db_conn.tell_fortune().await?;
                            let mut buf = String::with_capacity(2048);
                            let _ = write!(&mut buf, "{}", FortunesTemplate { fortunes });
                            let mut res = Response::new(Body::from(buf));
                            *res.headers_mut() = headers;
                            Ok::<_, Box<dyn std::error::Error + Send + Sync>>(res)
                        }
                        _ => {
                            let mut res = Response::new(Body::empty());
                            *res.status_mut() = hyper::StatusCode::NOT_FOUND;
                            *res.headers_mut() = headers;
                            Ok(res)
                        }
                    }
                }
            });
            // Spawn the `serve_connection` future into the runtime.
            handle2.spawn(http.serve_connection(socket, svc));
            Ok::<_, Box<dyn std::error::Error>>(())
        };

        handle.spawn(async move {
            let _ = db_fut.await;
        });
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
