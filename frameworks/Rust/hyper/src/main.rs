extern crate futures;
extern crate hyper;
extern crate pretty_env_logger;
#[macro_use]
extern crate log;
extern crate net2;
extern crate tokio_core;
extern crate tokio_proto;
extern crate num_cpus;

use hyper::{Get, StatusCode};
use hyper::header::{self, ContentLength, ContentType};
use hyper::server::{Http, Service, Request, Response};

use tokio_proto::TcpServer;

static HELLOWORLD: &'static [u8] = b"Hello, world!";

#[derive(Clone, Copy)]
struct TechEmpower;

impl Service for TechEmpower {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = ::futures::Finished<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        ::futures::finished(match (req.method(), req.path()) {
            (&Get, "/plaintext") => {
                use hyper::mime::{Mime,TopLevel,SubLevel,Attr,Value};
                Response::new()
                    .with_header(ContentLength(HELLOWORLD.len() as u64))
                    .with_header(ContentType(Mime(TopLevel::Text, SubLevel::Plain, vec![(Attr::Charset, Value::Utf8)])))
                    .with_header(header::Server("hyper/async".to_string()))
                    .with_body(HELLOWORLD)
            },
            _ => {
                Response::new()
                    .with_status(StatusCode::NotFound)
            }
        })
    }
}


fn main() {
    use std::net::SocketAddr;
    pretty_env_logger::init().unwrap();

    let addr: SocketAddr = "0.0.0.0:8080".parse().unwrap();
    let mut srv = TcpServer::new(Http::new(), addr);
    println!("Listening on http://{} using {} threads", addr, num_cpus::get());

    srv.threads(num_cpus::get());
    srv.serve(move || {
        Ok(TechEmpower)
    })
}
