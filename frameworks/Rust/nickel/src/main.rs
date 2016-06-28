#[macro_use]
extern crate nickel;
extern crate rustc_serialize;

use nickel::{Nickel, HttpRouter, MediaType};
use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Message {
    message: String,
}

fn main() {
    let mut server = Nickel::new();
    let mut router = Nickel::router();

    router.get("/json",
               middleware!{ |_, mut response|
        response.set(MediaType::Json);
        let message: Message = Message{
            message: "Hello, World!".to_string(),
        };
        json::encode(&message).unwrap()
    });

    router.get("/plaintext",
               middleware! { |_, mut response|
        response.set(MediaType::Txt);
        "Hello, World!"
    });

    server.utilize(router);
    server.listen("0.0.0.0:8080");
}
