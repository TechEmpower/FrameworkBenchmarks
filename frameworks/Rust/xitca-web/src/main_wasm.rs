mod ser;
mod util;

use xitca_web::{
    handler::{json::Json, text::Text},
    http::{header::SERVER, WebResponse},
    route::get,
    App,
};

fn main() -> std::io::Result<()> {
    let listener = std::env::var("FD_COUNT")
        .ok()
        .and_then(|v| v.parse().ok())
        .map(|fd| unsafe { std::os::wasi::io::FromRawFd::from_raw_fd(fd) })
        .expect("failed to parse FD_COUNT env");

    App::new()
        .at("/json", get(Json(ser::Message::new())))
        .at("/plaintext", get(Text("Hello, World!")))
        .map(header)
        .serve()
        .listen(listener)?
        .run()
        .wait()
}

fn header(mut res: WebResponse) -> WebResponse {
    res.headers_mut().append(SERVER, util::SERVER_HEADER_VALUE);
    res
}
