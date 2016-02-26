extern crate iron;
extern crate router;
extern crate rustc_serialize;

use iron::{Iron, Request, Response, IronResult};
use iron::status;
use router::Router;
use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Message {
    message: String,
}

fn main() {
    let mut router = Router::new();
    router.get("/json", json_handler);
    router.get("/plaintext", plaintext_handler);

    Iron::new(router).http("0.0.0.0:8080").unwrap();
}

fn json_handler(_: &mut Request) -> IronResult<Response> {
    let message: Message = Message { message: "Hello, World!".to_string() };
    Ok(Response::with((status::Ok, json::encode(&message).unwrap())))
}

fn plaintext_handler(_: &mut Request) -> IronResult<Response> {
    Ok(Response::with((status::Ok, "Hello, World!")))
}
