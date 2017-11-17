extern crate zap;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;

use std::io::Error as ZapError;
use zap::prelude::*;

// Our JSON struct
#[derive(Serialize, Deserialize)]
struct Message {
    content: String,
}

// Our handler
struct TechEmpower;

impl Handler for TechEmpower {
    type Request = Request;
    type Response = Response;
    type Error = ZapError;
    type Future = ZapResult;

    fn call(&self, req: Request) -> ZapResult {
        // Create new Response
        let mut resp = Response::new();
        let head = req.first();

        // Different content, depending on route
        if head.starts_with(b"GET /plaintext HTTP/1.1\r") {
            resp.header("Content-Type", "text/plain");

            resp.body_raw(b"Hello World");
        } else if head.starts_with(b"GET /json HTTP/1.1\r") {
            resp.header("Content-Type", "application/json");

            let message : Message = Message {
                content: "Hello World".to_string(),
            };

            resp.body_raw(&serde_json::to_vec(&message).unwrap());
        } else {
            resp.body_raw(b"Not Found");
            resp.status(404);
        }

        // Send response
        resp.ok()
    }
}

fn main() {
    // Set address
    let addr = "0.0.0.0:8080".parse().unwrap();

    // Create server
    let mut server = Server::new(Http, addr);

    // Set number of threads
    server.threads(8);

    // Serve the Handler
    server.serve(|| Ok(TechEmpower));
}
