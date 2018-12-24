extern crate gotham;
extern crate hyper;
extern crate mime;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use gotham::helpers::http::response;
use gotham::router::builder::*;
use gotham::state::State;
use hyper::{
    header::{HeaderValue, SERVER},
    Body, Response, StatusCode,
};

static HELLO_WORLD: &'static [u8] = b"Hello, world!";
static GOTHAM: &'static str = "Gotham";

#[derive(Serialize)]
struct Message<'a> {
    message: &'a str,
}

fn json(state: State) -> (State, Response<Body>) {
    let message = Message {
        message: "Hello, World!",
    };

    let body = serde_json::to_vec(&message).unwrap();
    let mut res = response::create_response(&state, StatusCode::OK, mime::APPLICATION_JSON, body);

    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static(GOTHAM));

    (state, res)
}

fn plaintext(state: State) -> (State, Response<Body>) {
    let mut res = response::create_response(&state, StatusCode::OK, mime::TEXT_PLAIN, HELLO_WORLD);

    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static(GOTHAM));

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
