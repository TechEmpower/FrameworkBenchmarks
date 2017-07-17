extern crate futures;
extern crate tokio_proto;
extern crate tokio_service;
extern crate hyper;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate num_cpus;
extern crate mime;

use tokio_proto::TcpServer;
use futures::future;
use hyper::Method::Get;
use hyper::header::{ContentLength, ContentType, Server};
use hyper::StatusCode::NotFound;
use hyper::server::{Http, Service, Request, Response};
use std::net::SocketAddr;

static HELLOWORLD: &'static [u8] = b"Hello, world!";

#[derive(Serialize)]
struct JsonResponse<'a> {
    message: &'a str,
}

struct TechEmpower;

impl Service for TechEmpower {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = ::futures::Finished<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        let response = match (req.method(), req.path()) {
            (&Get, "/plaintext") => {
                Response::new()
                    .with_header(ContentLength(HELLOWORLD.len() as u64))
                    .with_header(ContentType(mime::TEXT_PLAIN))
                    .with_body(HELLOWORLD)
            }
            (&Get, "/json") => {
                let rep = JsonResponse { message: "Hello, world!" };
                let rep_body = serde_json::to_vec(&rep).unwrap();
                Response::new()
                    .with_header(ContentLength(rep_body.len() as u64))
                    .with_header(ContentType(mime::APPLICATION_JSON))
                    .with_body(rep_body)
            }
            _ => Response::new().with_status(NotFound),
        };
        future::ok(response.with_header(Server::new("Hyper")))
    }
}

fn main() {
    let addr: SocketAddr = "0.0.0.0:8080".parse().unwrap();
    let mut srv = TcpServer::new(Http::new(), addr);
    println!("Listening on http://{} using {} threads",
             addr,
             num_cpus::get());

    srv.threads(num_cpus::get());
    srv.serve(move || Ok(TechEmpower))
}
