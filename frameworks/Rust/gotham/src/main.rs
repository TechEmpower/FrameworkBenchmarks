extern crate futures;
extern crate gotham;
extern crate hyper;
extern crate mime;
extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;

use hyper::{header, Response, StatusCode};
use gotham::http::response::create_response;
use gotham::state::State;
use gotham::router::Router;
use gotham::router::builder::*;


#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}


pub fn json(state: State) -> (State, Response) {
    let message = Message {
        message: "Hello, World!"
    };
    let body = serde_json::to_string(&message).unwrap();

    let mut res = create_response(
        &state,
        StatusCode::Ok,
        Some((body.into_bytes(), mime::APPLICATION_JSON)),
    );
    res.headers_mut().set(header::Server::new("Gotham"));

    (state, res)
}


pub fn plaintext(state: State) -> (State, Response) {
    let mut res = create_response(
        &state,
        StatusCode::Ok,
        Some((String::from("Hello, World!").into_bytes(), mime::TEXT_PLAIN)),
    );
    res.headers_mut().set(header::Server::new("Gotham"));

    (state, res)
}

fn router() -> Router {
    build_simple_router(|route| {
        route.get("/plaintext").to(plaintext);
        route.get("/json").to(json);
    })
}

pub fn main() {
    let addr = "0.0.0.0:8080";
    println!("Listening for requests at http://{}", addr);
    gotham::start(addr, router())
}
