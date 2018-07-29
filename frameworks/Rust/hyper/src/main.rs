extern crate futures;
extern crate hyper;
extern crate net2;
extern crate num_cpus;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate tokio_core;

use std::io;
use std::net::SocketAddr;
use std::thread;

use futures::{Future, Stream};

use hyper::{Body, Response, StatusCode};
use hyper::header::{CONTENT_LENGTH, CONTENT_TYPE, SERVER, HeaderValue};
use hyper::server::conn::Http;
use hyper::service::service_fn_ok;

use tokio_core::reactor::{Core, Handle};
use tokio_core::net::TcpListener;

static HELLO_WORLD: &'static [u8] = b"Hello, world!";

#[derive(Serialize)]
struct JsonResponse<'a> {
    message: &'a str,
}

fn server_thread() {
    // Configure HTTP options
    let mut http = Http::new();
    http.pipeline_flush(true);

    // It seems most of the other benchmarks create static header values
    // for performance, so just play by the same rules here...
    let plaintext_len = HeaderValue::from_static("13");
    let plaintext_ct = HeaderValue::from_static("text/plain");
    let json_len = HeaderValue::from_static("27");
    let json_ct = HeaderValue::from_static("application/json");
    let server_header = HeaderValue::from_static("hyper");

    // This will create our `Service` to handle an individual connection.
    let new_svc = move || {
        // Gotta clone these to be able to move into the Service...
        let plaintext_len = plaintext_len.clone();
        let plaintext_ct = plaintext_ct.clone();
        let json_len = json_len.clone();
        let json_ct = json_ct.clone();
        let server_header = server_header.clone();

        // This is the `Service` that will handle the connection.
        // `service_fn_ok` is a helper to convert a function that
        // returns a Response into a `Service`.
        service_fn_ok(move |req| {
            let (req, _body) = req.into_parts();
            // For speed, reuse the allocated header map from the request,
            // instead of allocating a new one. Because.
            let mut headers = req.headers;
            headers.clear();

            let body = match req.uri.path() {
                // Apparently, other benchmarks don't check the method, so we
                // don't either. Yay?
                "/plaintext" => {
                    headers.insert(CONTENT_LENGTH, plaintext_len.clone());
                    headers.insert(CONTENT_TYPE, plaintext_ct.clone());
                    Body::from(HELLO_WORLD)
                }
                "/json" => {
                    let rep = JsonResponse { message: "Hello, world!" };
                    let rep_body = serde_json::to_vec(&rep).unwrap();
                    headers.insert(CONTENT_LENGTH, json_len.clone());
                    headers.insert(CONTENT_TYPE, json_ct.clone());
                    Body::from(rep_body)
                }
                _ => {
                    let mut res = Response::new(Body::empty());
                    *res.status_mut() = StatusCode::NOT_FOUND;
                    *res.headers_mut() = headers;
                    return res;
                },
            };

            headers.insert(SERVER, server_header.clone());

            let mut res = Response::new(body);
            *res.headers_mut() = headers;
            res
        })
    };

    // Our event loop...
    let mut core = Core::new().expect("core");
    let handle = core.handle();

    // Bind to 0.0.0.0:8080
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    let tcp = reuse_listener(&addr, &handle)
        .expect("couldn't bind to addr");

    // For every accepted connection, spawn an HTTP task
    let server = tcp.incoming()
        .for_each(move |(sock, _addr)| {
            let _ = sock.set_nodelay(true);
            let conn = http.serve_connection(sock, new_svc())
                .map_err(|e| eprintln!("connection error: {}", e));

            handle.spawn(conn);

            Ok(())
        })
        .map_err(|e| eprintln!("accept error: {}", e));

    core.run(server).expect("server");
}


fn reuse_listener(addr: &SocketAddr, handle: &Handle) -> io::Result<TcpListener> {
    let builder = match *addr {
        SocketAddr::V4(_) => net2::TcpBuilder::new_v4()?,
        SocketAddr::V6(_) => net2::TcpBuilder::new_v6()?,
    };

    #[cfg(unix)]
    {
        use net2::unix::UnixTcpBuilderExt;
        if let Err(e) = builder.reuse_port(true) {
            eprintln!("error setting SO_REUSEPORT: {}", e);
        }
    }

    builder.reuse_address(true)?;
    builder.bind(addr)?;
    builder.listen(1024).and_then(|l| {
        TcpListener::from_listener(l, addr, handle)
    })
}

fn main() {
    // Spawn a thread for each available core, minus one, since we'll
    // reuse the main thread as a server thread as well.
    for _ in 1..num_cpus::get() {
        thread::spawn(server_thread);
    }
    server_thread();
}
