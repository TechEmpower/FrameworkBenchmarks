extern crate hyper;

use hyper::server::{Server, Request, Response};
use hyper::uri::RequestUri;
use hyper::header::ContentType;
use hyper::header;

const HELLO_WORLD: &'static [u8; 13] = b"Hello, World!";

fn main() {
    Server::http("0.0.0.0:8080").unwrap().handle(handler).unwrap();
}

fn handler(req: Request, mut res: Response) {
    match (req.method, req.uri) {
        (hyper::Get, RequestUri::AbsolutePath(ref path)) if path == "/plaintext" => {
            res.headers_mut().set(ContentType("text/plain".parse().unwrap()));
            res.headers_mut().set(header::Server("Hyper".to_owned()));

            res.send(HELLO_WORLD).unwrap();
        }
        _ => (),
    }
}
