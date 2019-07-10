extern crate may;
extern crate num_cpus;
#[macro_use]
extern crate serde_derive;
extern crate may_minihttp;
extern crate serde_json;

use may_minihttp::{HttpServer, HttpService, Request, Response};
use std::io;

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
                *resp.body_mut() = serde_json::to_vec(&Message {
                    message: "Hello, World!",
                })
                .unwrap();
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
    may::config()
        .set_io_workers(num_cpus::get())
        .set_pool_capacity(20000)
        .set_stack_size(0x800);

    let mut servers = Vec::new();
    for _ in 0..num_cpus::get() {
        let server = HttpServer(Techempower).start("0.0.0.0:8080").unwrap();
        servers.push(server);
    }

    for server in servers {
        server.join().unwrap();
    }
}
