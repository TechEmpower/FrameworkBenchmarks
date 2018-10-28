extern crate gotham;
extern crate hyper;
extern crate mime;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use gotham::http::response::create_response;
use gotham::router::builder::*;
use gotham::state::State;
use hyper::{header, Response, StatusCode};

static HELLO_WORLD: &'static [u8] = b"Hello, world!";

#[derive(Serialize)]
struct Message<'a> {
    message: &'a str,
}

fn json(state: State) -> (State, Response) {
    let message = Message {
        message: "Hello, World!",
    };

    let body = serde_json::to_vec(&message).unwrap();
    let mut res = create_response(&state, StatusCode::Ok, Some((body, mime::APPLICATION_JSON)));

    res.headers_mut().set(header::Server::new("Gotham"));

    (state, res)
}

fn plaintext(state: State) -> (State, Response) {
    let mut res = create_response(
        &state,
        StatusCode::Ok,
        Some((HELLO_WORLD.to_vec(), mime::TEXT_PLAIN)),
    );

    res.headers_mut().set(header::Server::new("Gotham"));

    (state, res)
}

pub fn main() {
    let addr = "0.0.0.0:8080";
    println!("Listening for requests at http://{}", addr);

    let router = build_simple_router(|route| {
        route.get("/plaintext").to(plaintext);
        route.get("/json").to(json);
    });

    gotham::start(addr, router)
}
