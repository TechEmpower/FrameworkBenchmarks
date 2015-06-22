extern crate hyper;

use hyper::server::{Server, Request, Response};
use hyper::status::StatusCode;
use hyper::uri::RequestUri;
use hyper::header::ContentType;
use hyper::header::ContentLength;
use hyper::header;

const HELLO_WORLD: &'static [u8; 14] = b"Hello, World!\n";

fn main() {
    Server::http(|req: Request, mut res: Response| {
        match (req.method, req.uri) {
            (hyper::Get, RequestUri::AbsolutePath(ref path)) if path == "/plaintext" => {
                res.headers_mut().set(ContentType::plaintext());
                res.headers_mut().set(header::Server("Hyper".to_owned()));

                res.send(HELLO_WORLD).unwrap();
            }
            _ => (),
        };
    }).listen("0.0.0.0:8080").unwrap();
}
