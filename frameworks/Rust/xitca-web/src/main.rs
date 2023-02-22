#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod ser;
mod util;

use std::{cell::RefCell, convert::Infallible, error::Error, fmt::Debug, io};

use xitca_http::{
    body::Once,
    bytes::{BufMutWriter, Bytes, BytesMut},
    config::HttpServiceConfig,
    h1::RequestBody,
    http::{
        self,
        const_header_value::{JSON, TEXT, TEXT_HTML_UTF8},
        header::{CONTENT_TYPE, SERVER},
        IntoResponse, RequestExt,
    },
    util::service::{
        context::{object::ContextObjectConstructor, Context, ContextBuilder},
        route::get,
        GenericRouter,
    },
    HttpServiceBuilder,
};
use xitca_service::{fn_service, Service, ServiceExt};

use self::db::Client;
use self::ser::Message;
use self::util::{QueryParse, DB_URL, SERVER_HEADER_VALUE};

type Response = http::Response<Once<Bytes>>;
type Request = http::Request<RequestExt<RequestBody>>;
type Ctx<'a> = Context<'a, Request, State>;

fn main() -> io::Result<()> {
    xitca_server::Builder::new()
        .bind("xitca-web", "0.0.0.0:8080", || {
            HttpServiceBuilder::h1(
                ContextBuilder::new(|| async {
                    db::create(DB_URL).await.map(|client| State {
                        client,
                        write_buf: RefCell::new(BytesMut::new()),
                    })
                })
                .service(
                    GenericRouter::with_custom_object::<ContextObjectConstructor<_, _>>()
                        .insert("/plaintext", get(fn_service(plain_text)))
                        .insert("/json", get(fn_service(json)))
                        .insert("/db", get(fn_service(db)))
                        .insert("/fortunes", get(fn_service(fortunes)))
                        .insert("/queries", get(fn_service(queries)))
                        .insert("/updates", get(fn_service(updates)))
                        .enclosed_fn(middleware_fn),
                ),
            )
            .config(
                HttpServiceConfig::new()
                    .disable_vectored_write()
                    .max_request_headers::<8>(),
            )
        })?
        .build()
        .wait()
}

async fn middleware_fn<S, E>(service: &S, req: Ctx<'_>) -> Result<Response, Infallible>
where
    S: for<'c> Service<Ctx<'c>, Response = Response, Error = E>,
    E: Debug,
{
    let mut res = service.call(req).await.unwrap();
    res.headers_mut().append(SERVER, SERVER_HEADER_VALUE);
    Ok(res)
}

async fn plain_text(ctx: Ctx<'_>) -> Result<Response, Box<dyn Error>> {
    let (req, _) = ctx.into_parts();
    let mut res = req.into_response(Bytes::from_static(b"Hello, World!"));
    res.headers_mut().append(CONTENT_TYPE, TEXT);
    Ok(res)
}

async fn json(ctx: Ctx<'_>) -> Result<Response, Box<dyn Error>> {
    let (req, state) = ctx.into_parts();
    _json(req, state, &Message::new())
}

async fn db(ctx: Ctx<'_>) -> Result<Response, Box<dyn Error>> {
    let (req, state) = ctx.into_parts();
    let world = state.client.get_world().await?;
    _json(req, state, &world)
}

async fn fortunes(ctx: Ctx<'_>) -> Result<Response, Box<dyn Error>> {
    let (req, state) = ctx.into_parts();
    use sailfish::TemplateOnce;
    let fortunes = state.client.tell_fortune().await?.render_once()?;
    let mut res = req.into_response(Bytes::from(fortunes));
    res.headers_mut().append(CONTENT_TYPE, TEXT_HTML_UTF8);
    Ok(res)
}

async fn queries(ctx: Ctx<'_>) -> Result<Response, Box<dyn Error>> {
    let (req, state) = ctx.into_parts();
    let num = req.uri().query().parse_query();
    let worlds = state.client.get_worlds(num).await?;
    _json(req, state, worlds.as_slice())
}

async fn updates(ctx: Ctx<'_>) -> Result<Response, Box<dyn Error>> {
    let (req, state) = ctx.into_parts();
    let num = req.uri().query().parse_query();
    let worlds = state.client.update(num).await?;
    _json(req, state, worlds.as_slice())
}

fn _json<S>(req: Request, state: &State, value: &S) -> Result<Response, Box<dyn Error>>
where
    S: ?Sized + serde::Serialize,
{
    let mut buf = state.write_buf.borrow_mut();
    serde_json::to_writer(BufMutWriter(&mut *buf), value)?;
    let body = buf.split().freeze();
    let mut res = req.into_response(body);
    res.headers_mut().append(CONTENT_TYPE, JSON);
    Ok(res)
}

struct State {
    client: Client,
    write_buf: RefCell<BytesMut>,
}
