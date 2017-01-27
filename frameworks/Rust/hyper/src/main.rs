extern crate futures;
extern crate hyper;
extern crate pretty_env_logger;
#[macro_use]
extern crate log;
extern crate net2;
extern crate tokio_core;
extern crate num_cpus;

use hyper::{Get, StatusCode};
use hyper::header::{ContentLength, ContentType};
use hyper::server::{Server, Service, Request, Response};

use net2::TcpBuilder;
use net2::unix::UnixTcpBuilderExt;
use tokio_core::net::TcpListener;

static HELLOWORLD: &'static [u8] = b"Hello, world!";

#[derive(Clone, Copy)]
struct TechEmpower;

impl Service for TechEmpower {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = ::futures::Finished<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        ::futures::finished(match (req.method(), req.path()) {
            (&Get, "/plaintext") => {
                use hyper::mime::{Mime,TopLevel,SubLevel,Attr,Value};
                Response::new()
                    .with_header(ContentLength(HELLOWORLD.len() as u64))
                    .with_header(ContentType(Mime(TopLevel::Text, SubLevel::Plain, vec![(Attr::Charset, Value::Utf8)])))
                    .with_body(HELLOWORLD)
	    },
            _ => {
                Response::new()
                    .with_status(StatusCode::NotFound)
            }
        })
    }
}


fn main() {
    use std::net::SocketAddr;
    pretty_env_logger::init().unwrap();
    let addr: SocketAddr = "127.0.0.1:8080".parse().unwrap();
    let mut threads = vec![];
    for i in 0..num_cpus::get_physical() {
        use std::thread;
        let i = i;
        let handle = thread::spawn(move|| {
            let (listening, server) = Server::standalone(|tokio| {
		    let listener = TcpBuilder::new_v4()?.reuse_port(true)?.bind(addr)?.listen(10000)?;
		    let addr = try!(listener.local_addr());
		    let listener = try!(TcpListener::from_listener(listener, &addr, tokio));
		    Server::new(listener.incoming(), addr).handle(|| Ok(TechEmpower), tokio)
	    }).unwrap();
            println!("Listening {} on http://{}", i, listening);
            server.run();
        });
        threads.push(handle);
    }
    for t in threads {
        t.join().unwrap();
    }
}
