extern crate iron;
extern crate router;
extern crate rustc_serialize;

use iron::{Iron, Request, Response, IronResult};
use iron::status;
use router::Router;
use rustc_serialize::json;
use iron::mime::Mime;
use iron::headers::Server;
use iron::modifiers::Header;

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
    let mime: Mime = "application/json".parse().unwrap();
    let server = Header(Server(String::from("Iron")));
    Ok(Response::with((status::Ok, json::encode(&message).unwrap(), mime, server)))
}

fn plaintext_handler(_: &mut Request) -> IronResult<Response> {
    let server = Header(Server(String::from("Iron")));
    Ok(Response::with((status::Ok, "Hello, World!", server)))
}
