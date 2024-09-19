// used as reference of if/how moving from epoll to io-uring(or mixture of the two) make sense for
// network io.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod ser;
mod util;

use std::{convert::Infallible, io};

use xitca_http::{
    body::ResponseBody,
    http::{self, header::SERVER, StatusCode},
    HttpServiceBuilder,
};
use xitca_service::{fn_service, ServiceExt};

use self::{
    ser::{error_response, IntoResponse, Message, Request},
    util::{context_mw, Ctx, QueryParse, SERVER_HEADER_VALUE},
};

fn main() -> io::Result<()> {
    let service = fn_service(handler)
        .enclosed(context_mw())
        .enclosed(HttpServiceBuilder::h1().io_uring());
    xitca_server::Builder::new()
        .bind("xitca-iou", "0.0.0.0:8080", service)?
        .build()
        .wait()
}

async fn handler<B>(ctx: Ctx<'_, Request<B>>) -> Result<http::Response<ResponseBody>, Infallible> {
    let (req, state) = ctx.into_parts();
    let mut res = match req.uri().path() {
        "/plaintext" => req.text_response().unwrap(),
        "/json" => req.json_response(state, &Message::new()).unwrap(),
        "/db" => {
            let world = state.client.get_world().await.unwrap();
            req.json_response(state, &world).unwrap()
        }
        "/queries" => {
            let num = req.uri().query().parse_query();
            let worlds = state.client.get_worlds(num).await.unwrap();
            req.json_response(state, &worlds).unwrap()
        }
        "/updates" => {
            let num = req.uri().query().parse_query();
            let worlds = state.client.update(num).await.unwrap();
            req.json_response(state, &worlds).unwrap()
        }
        "/fortunes" => {
            use sailfish::TemplateOnce;
            let fortunes = state.client.tell_fortune().await.unwrap().render_once().unwrap();
            req.html_response(fortunes).unwrap()
        }
        _ => error_response(StatusCode::NOT_FOUND),
    };
    res.headers_mut().insert(SERVER, SERVER_HEADER_VALUE);
    Ok(res.map(Into::into))
}
