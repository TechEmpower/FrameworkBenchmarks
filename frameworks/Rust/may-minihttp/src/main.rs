extern crate may;
extern crate num_cpus;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate may_minihttp;

use std::io;
use may_minihttp::{HttpServer, HttpService, Request, Response};

#[derive(Serialize)]
struct Message<'a> {
    message: &'a str,
}

struct Techempower;

impl HttpService for Techempower {
    fn call(&self, req: Request) -> io::Result<Response> {
        let mut resp = Response::new();

        // Bare-bones router
        match req.path() {
            "/json" => {
                resp.header("Content-Type", "application/json");
                *resp.body_mut() =
                    serde_json::to_vec(&Message { message: "Hello, World!" }).unwrap();
            }
            "/plaintext" => {
                resp.header("Content-Type", "text/plain")
                    .body("Hello, World!");
            }
            _ => {
                resp.status_code(404, "Not Found");
            }
        }

        Ok(resp)
    }
}

fn main() {
    may::config().set_io_workers(num_cpus::get());
    let server = HttpServer(Techempower).start("0.0.0.0:8080").unwrap();
    server.join().unwrap();
}
