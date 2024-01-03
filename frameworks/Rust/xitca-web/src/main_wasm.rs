mod ser;
mod util;

use std::{env, io, net::TcpListener, os::wasi::io::FromRawFd};

use xitca_web::{
    handler::{handler_service, json::Json},
    http::{header::SERVER, WebResponse},
    route::get,
    service::Service,
    App, WebContext,
};

fn main() -> io::Result<()> {
    let fd = env::var("FD_COUNT")
        .ok()
        .and_then(|var| var.parse().ok())
        .expect("failed to parse FD_COUNT env");

    let listener = unsafe { TcpListener::from_raw_fd(fd) };

    App::new()
        .at(
            "/json",
            get(handler_service(|| async { Json(ser::Message::new()) })),
        )
        .at(
            "/plaintext",
            get(handler_service(|| async { "Hello, World!" })),
        )
        .enclosed_fn(middleware_fn)
        .serve()
        .listen(listener)?
        .run()
        .wait()
}

async fn middleware_fn<S, E>(service: &S, ctx: WebContext<'_>) -> Result<WebResponse, E>
where
    S: for<'r> Service<WebContext<'r>, Response = WebResponse, Error = E>,
{
    service.call(ctx).await.map(|mut res| {
        res.headers_mut().append(SERVER, util::SERVER_HEADER_VALUE);
        res
    })
}
