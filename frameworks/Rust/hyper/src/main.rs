extern crate futures;
extern crate hyper;
extern crate num_cpus;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
//extern crate tokio;
extern crate tokio_proto;

use std::env;
use std::net::SocketAddr;
use std::process;

use futures::{future}; //, Future, Stream};

use hyper::Method::Get;
use hyper::header::{ContentLength, ContentType, Server};
use hyper::StatusCode::NotFound;
use hyper::server::{Http, Service, Request, Response};

//use tokio::net::TcpListener;
use tokio_proto::TcpServer;

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
    type Future = future::FutureResult<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        let response = match (req.method(), req.path()) {
            (&Get, "/plaintext") => {
                Response::new()
                    .with_header(ContentLength(HELLOWORLD.len() as u64))
                    .with_header(ContentType::text())
                    .with_body(HELLOWORLD)
            }
            (&Get, "/json") => {
                let rep = JsonResponse { message: "Hello, world!" };
                let rep_body = serde_json::to_vec(&rep).unwrap();
                Response::new()
                    .with_header(ContentLength(rep_body.len() as u64))
                    .with_header(ContentType::json())
                    .with_body(rep_body)
            }
            _ => Response::new().with_status(NotFound),
        };
        future::ok(response.with_header(Server::new("Hyper")))
    }
}

fn configure() -> Http {
    let pipeline = {
        let mut args = env::args();
        args.next().expect("first arg is this binary");

        args.next()
            .map(|arg| {
                if arg == "pipeline" {
                    true
                } else {
                    eprintln!("unknown second argument: {:?}", arg);
                    process::exit(1);
                }
            })
            .unwrap_or(false)
    };

    // Set our hyper options
    // (pipeline is desired for the plaintext route)
    let mut http = Http::<hyper::Chunk>::new();
    http.pipeline(pipeline);
    http
}

fn main() {
    // Check for some runtime configuration
    let http = configure();

    // Bind to 0.0.0.0:8080
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));

    let mut srv = TcpServer::new(http, addr);
    srv.threads(num_cpus::get());

    println!("Listening on http://{}", addr);
    srv.serve(|| Ok(TechEmpower));
}

/* This is the future, but there's still a few blockers in new tokio,
 * so disable this for now while we work them out.
fn main() {
    // Check for some runtime configuration
    let http = configure();

    // Bind to 0.0.0.0:8080
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    let tcp = TcpListener::bind(&addr)
        .expect("couldn't bind to addr");

    // For every accepted connection, spawn an HTTP task
    let server = tcp.incoming()
        .for_each(move |sock| {
            let _ = sock.set_nodelay(true);
            let conn = http.serve_connection(sock, TechEmpower)
                .map_err(|e| eprintln!("connection error: {}", e));

            tokio::spawn(conn);

            Ok(())
        })
        .map_err(|e| eprintln!("accept error: {}", e));

    println!("Listening on http://{}", addr);
    tokio::run(server);
}
*/
