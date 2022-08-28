#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod ser;
mod util;

use std::{
    cell::RefCell,
    convert::Infallible,
    error::Error,
    fmt::Debug,
    sync::{Arc, Mutex},
};

use simd_json_derive::Serialize;
use xitca_http::{
    body::Once,
    bytes::{BufMutWriter, Bytes, BytesMut},
    config::HttpServiceConfig,
    h1::RequestBody,
    http::{
        self,
        const_header_value::{JSON, TEXT, TEXT_HTML_UTF8},
        header::{CONTENT_TYPE, SERVER},
        IntoResponse,
    },
    request,
    util::{
        middleware::TcpConfig,
        service::{
            context::{object::ContextObjectConstructor, Context, ContextBuilder},
            route::get,
            GenericRouter,
        },
    },
    HttpServiceBuilder,
};
use xitca_service::{fn_service, BuildServiceExt, Service};

use self::db::Client;
use self::ser::Message;
use self::util::{QueryParse, SERVER_HEADER_VALUE};

type Response = http::Response<Once<Bytes>>;
type Request = request::Request<RequestBody>;

type Ctx<'a> = Context<'a, Request, State>;

fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
    let cores = core_affinity::get_core_ids().unwrap_or_default();
    let cores = Arc::new(Mutex::new(cores));

    let db_url = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    let builder = || {
        let config = HttpServiceConfig::new()
            .disable_vectored_write()
            .max_request_headers::<8>();

        let router = GenericRouter::with_custom_object::<ContextObjectConstructor<_, _>>()
            .insert("/plaintext", get(fn_service(plain_text)))
            .insert("/json", get(fn_service(json)))
            .insert("/db", get(fn_service(db)))
            .insert("/fortunes", get(fn_service(fortunes)))
            .insert("/queries", get(fn_service(queries)))
            .insert("/updates", get(fn_service(updates)))
            .enclosed_fn(middleware_fn);

        let service = ContextBuilder::new(|| async {
            let client = db::create(db_url).await;
            let write_buf = RefCell::new(BytesMut::new());
            Ok::<_, Infallible>(State { client, write_buf })
        })
        .service(router);

        let tcp_config = TcpConfig::new().set_nodelay(true);

        HttpServiceBuilder::h1(service)
            .config(config)
            .enclosed(tcp_config)
    };

    xitca_server::Builder::new()
        .on_worker_start(move || {
            if let Some(core) = cores.lock().unwrap().pop() {
                core_affinity::set_for_current(core);
            }
            async {}
        })
        .bind("xitca-web", "0.0.0.0:8080", builder)?
        .build()
        .wait()
        .map_err(Into::into)
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
    S: ?Sized + Serialize,
{
    let mut buf = state.write_buf.borrow_mut();
    value.json_write(&mut BufMutWriter(&mut *buf)).unwrap();
    let body = buf.split().freeze();
    let mut res = req.into_response(body);
    res.headers_mut().append(CONTENT_TYPE, JSON);
    Ok(res)
}

struct State {
    client: Client,
    write_buf: RefCell<BytesMut>,
}
