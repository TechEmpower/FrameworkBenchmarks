// temporary allocator for tracking overhead between xitca-web and xitca-web [barebone] bench.
// remove it before official run
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod db_pool;
mod ser;
mod util;

use xitca_http::{
    HttpServiceBuilder,
    body::Once,
    bytes::Bytes,
    h1::RequestBody,
    http::{
        self, HeaderValue, IntoResponse as _, RequestExt, StatusCode,
        const_header_value::{JSON, TEXT_HTML_UTF8, TEXT_UTF8},
        header::{CONTENT_TYPE, SERVER},
    },
    util::{
        middleware::context::{Context, ContextBuilder},
        service::{
            route::get,
            router::{Router, RouterError},
        },
    },
};
use xitca_service::{Service, ServiceExt, fn_service};

use ser::{HELLO, Message};
use util::{HandleResult, QueryParse};

type Request<B> = http::Request<RequestExt<B>>;

type Response = http::Response<Once<Bytes>>;

type Ctx<'a> = Context<'a, Request<RequestBody>, db_pool::Client>;

fn main() -> std::io::Result<()> {
    let service = Router::new()
        .insert(
            "/plaintext",
            get(fn_service(async |ctx: Ctx| {
                let (req, _) = ctx.into_parts();
                let mut res = req.into_response(const { Bytes::from_static(HELLO.as_bytes()) });
                res.headers_mut().insert(CONTENT_TYPE, TEXT_UTF8);
                Ok(res)
            })),
        )
        .insert(
            "/json",
            get(fn_service(async |ctx: Ctx| {
                let (req, _) = ctx.into_parts();
                json_response(req, Message::HELLO)
            })),
        )
        .insert(
            "/db",
            get(fn_service(async |ctx: Ctx| {
                let (req, cli) = ctx.into_parts();
                cli.db().await.and_then(|w| json_response(req, &w))
            })),
        )
        .insert(
            "/fortunes",
            get(fn_service(async |ctx: Ctx| {
                let (req, cli) = ctx.into_parts();
                let fortunes = cli.fortunes().await?.render_once()?;
                let mut res = req.into_response(Bytes::from(fortunes));
                res.headers_mut().insert(CONTENT_TYPE, TEXT_HTML_UTF8);
                Ok(res)
            })),
        )
        .insert(
            "/queries",
            get(fn_service(async |ctx: Ctx| {
                let (req, cli) = ctx.into_parts();
                let num = req.uri().query().parse_query();
                cli.queries(num).await.and_then(|w| json_response(req, &w))
            })),
        )
        .insert(
            "/updates",
            get(fn_service(async |ctx: Ctx| {
                let (req, cli) = ctx.into_parts();
                let num = req.uri().query().parse_query();
                cli.updates(num).await.and_then(|w| json_response(req, &w))
            })),
        )
        .enclosed(ContextBuilder::new(db_pool::Client::create))
        .enclosed_fn(async |service, req| {
            let mut res = service.call(req).await.unwrap_or_else(error_handler);
            res.headers_mut().insert(SERVER, HeaderValue::from_static("x"));
            Ok::<_, core::convert::Infallible>(res)
        })
        .enclosed(HttpServiceBuilder::h1().io_uring());
    xitca_server::Builder::new()
        .bind("xitca-web", "0.0.0.0:8080", service)?
        .build()
        .wait()
}

#[cold]
#[inline(never)]
fn error_handler(e: RouterError<util::Error>) -> Response {
    let status = match e {
        RouterError::Match(_) => StatusCode::NOT_FOUND,
        RouterError::NotAllowed(_) => StatusCode::METHOD_NOT_ALLOWED,
        RouterError::Service(e) => {
            eprintln!("Internal Error: {e}");
            StatusCode::INTERNAL_SERVER_ERROR
        }
    };
    http::Response::builder()
        .status(status)
        .body(Once::new(Bytes::new()))
        .unwrap()
}

fn json_response<Ext>(req: Request<Ext>, val: &impl serde_core::Serialize) -> HandleResult<Response> {
    let buf = ser::json_serialize(val)?;
    let mut res = req.into_response(Bytes::from(buf));
    res.headers_mut().insert(CONTENT_TYPE, JSON);
    Ok(res)
}
