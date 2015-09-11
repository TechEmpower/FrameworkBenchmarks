extern crate iron;
extern crate router;
extern crate rustc_serialize;

use iron::{Iron, Request, Response, IronResult};
use iron::status;
use router::{Router};
use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Message {
    message: String,
}

fn main() {
    let mut router = Router::new();
    router.get("/json", jsonHandler);
    router.get("/plaintext", plaintextHandler);

    Iron::new(router).http("0.0.0.0:8080").unwrap();
}

fn jsonHandler(req: &mut Request) -> IronResult<Response> {
    let message: Message = Message{
        message: "Hello, World!".to_string(),
    };
    Ok(Response::with((status::Ok, json::encode(&message).unwrap())))
}

fn plaintextHandler(req: &mut Request) -> IronResult<Response> {
    Ok(Response::with((status::Ok, "Hello, World!")))
}
