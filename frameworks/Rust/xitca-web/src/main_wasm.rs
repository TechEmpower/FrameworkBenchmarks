mod util;

use std::{env, io, net::TcpListener, os::wasi::io::FromRawFd};

use xitca_web::{
    dev::service::Service, handler::handler_service, http::header::SERVER, request::WebRequest,
    response::WebResponse, route::get, App, HttpServer,
};

use self::util::SERVER_HEADER_VALUE;

fn main() -> io::Result<()> {
    let fd = env::var("FD_COUNT")
        .ok()
        .and_then(|var| var.parse().ok())
        .expect("failed to parse FD_COUNT env");

    let listener = unsafe { TcpListener::from_raw_fd(fd) };

    HttpServer::new(|| {
        App::new()
            .at(
                "/plaintext",
                get(handler_service(|| async { "Hello, World!" })),
            )
            .enclosed_fn(middleware_fn)
            .finish()
    })
    .listen(listener)?
    .run()
    .wait()
}

async fn middleware_fn<S, E>(service: &S, ctx: WebRequest<'_>) -> Result<WebResponse, E>
where
    S: for<'r> Service<WebRequest<'r>, Response = WebResponse, Error = E>,
{
    service.call(ctx).await.map(|mut res| {
        res.headers_mut().append(SERVER, SERVER_HEADER_VALUE);
        res
    })
}
