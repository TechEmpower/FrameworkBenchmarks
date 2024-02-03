mod db;
mod ser;
mod util;

use xitca_http::{
    body::Once,
    bytes::Bytes,
    h1::RequestBody,
    http::{
        self,
        const_header_value::{TEXT, TEXT_HTML_UTF8},
        header::{CONTENT_TYPE, SERVER},
        IntoResponse, RequestExt,
    },
    util::service::{route::get, router::Router},
    HttpServiceBuilder,
};
use xitca_service::{fn_service, Service, ServiceExt};

use ser::{json_response, Message};
use util::{context_mw, HandleResult, QueryParse, SERVER_HEADER_VALUE};

type Request = http::Request<RequestExt<RequestBody>>;
type Response = http::Response<Once<Bytes>>;
type Ctx<'a> = util::Ctx<'a, Request>;

fn main() -> std::io::Result<()> {
    let service = Router::new()
        .insert("/plaintext", get(fn_service(plain_text)))
        .insert("/json", get(fn_service(json)))
        .insert("/db", get(fn_service(db)))
        .insert("/fortunes", get(fn_service(fortunes)))
        .insert("/queries", get(fn_service(queries)))
        .insert("/updates", get(fn_service(updates)))
        .enclosed_fn(middleware_fn)
        .enclosed(context_mw())
        .enclosed(HttpServiceBuilder::h1().io_uring());
    xitca_server::Builder::new()
        .bind("xitca-web", "0.0.0.0:8080", service)?
        .build()
        .wait()
}

async fn middleware_fn<S, E>(service: &S, req: Ctx<'_>) -> Result<Response, E>
where
    S: for<'c> Service<Ctx<'c>, Response = Response, Error = E>,
{
    service.call(req).await.map(|mut res| {
        res.headers_mut().insert(SERVER, SERVER_HEADER_VALUE);
        res
    })
}

async fn plain_text(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, _) = ctx.into_parts();
    let mut res = req.into_response(Bytes::from_static(b"Hello, World!"));
    res.headers_mut().insert(CONTENT_TYPE, TEXT);
    Ok(res)
}

async fn json(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    json_response(req, &mut state.write_buf.borrow_mut(), &Message::new())
}

async fn db(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    let world = state.client.get_world().await?;
    json_response(req, &mut state.write_buf.borrow_mut(), &world)
}

async fn fortunes(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    use sailfish::TemplateOnce;
    let fortunes = state.client.tell_fortune().await?.render_once()?;
    let mut res = req.into_response(Bytes::from(fortunes));
    res.headers_mut().insert(CONTENT_TYPE, TEXT_HTML_UTF8);
    Ok(res)
}

async fn queries(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    let num = req.uri().query().parse_query();
    let worlds = state.client.get_worlds(num).await?;
    json_response(req, &mut state.write_buf.borrow_mut(), worlds.as_slice())
}

async fn updates(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    let num = req.uri().query().parse_query();
    let worlds = state.client.update(num).await?;
    json_response(req, &mut state.write_buf.borrow_mut(), worlds.as_slice())
}
