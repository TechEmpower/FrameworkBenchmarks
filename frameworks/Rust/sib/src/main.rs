use sib::network::http::{
    request::Request,
    response::Response,
    server::{H1ServiceFactory, HttpService},
};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

struct H1Server<T>(pub T);

struct H1Service;

impl HttpService for H1Service {
    fn call(&mut self, _req: Request, rsp: &mut Response) -> std::io::Result<()> {
        const HELLO_WORLD: &str = "Hello, World!";
        rsp.status_code(200, "OK")
            .header("Content-Type: text/plain")
            .body(HELLO_WORLD.as_bytes().into());
        Ok(())
    }
}

impl H1ServiceFactory for H1Server<H1Service> {
    type Service = H1Service;

    fn new_service(&self, _id: usize) -> H1Service {
        H1Service
    }
}

fn main() {
    // Pick a port and start the server
    let addr = "0.0.0.0:8080";
    H1Server(H1Service)
        .start(&addr)
        .expect("h1 server failed to start")
        .join()
        .expect("h1 failed on joining thread");
}
