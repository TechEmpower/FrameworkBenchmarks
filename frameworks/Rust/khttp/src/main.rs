use khttp::{Headers, Method::*, Server};
use yarte::Serialize;

#[derive(Serialize)]
struct HelloMessage {
    message: &'static str,
}

fn main() {
    let mut app = Server::builder("0.0.0.0:8080").unwrap();

    app.route(Get, "/plaintext", |_ctx, res| {
        // headers
        let mut headers = Headers::new();
        headers.add(Headers::CONTENT_TYPE, b"text/plain");
        headers.add("server", b"khttp");

        // response
        res.ok(&headers, "Hello, World!")
    });

    app.route(Get, "/json", |_ctx, res| {
        // headers
        let mut headers = Headers::new();
        headers.add(Headers::CONTENT_TYPE, b"application/json");
        headers.add("server", b"khttp");

        // body
        let msg = HelloMessage {
            message: "Hello, World!",
        };
        let mut buf = Vec::with_capacity(32);
        msg.to_bytes_mut(&mut buf);

        // response
        res.ok(&headers, buf)
    });

    app.build().serve_epoll().unwrap();
}
