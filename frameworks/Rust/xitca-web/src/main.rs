mod db;
mod ser;
mod util;

use xitca_http::{
    HttpServiceBuilder,
    h1::RequestBody,
    http::{StatusCode, header::SERVER},
    util::{
        middleware::context::{Context, ContextBuilder},
        service::{
            route::get,
            router::{Router, RouterError},
        },
    },
};
use xitca_service::{Service, ServiceExt, fn_service};

use db::Client;
use ser::{IntoResponse, Message, Request, Response, error_response};
use util::{QueryParse, SERVER_HEADER_VALUE, State};

type Ctx<'a> = Context<'a, Request<RequestBody>, State<Client>>;

fn main() -> std::io::Result<()> {
    let service = Router::new()
        .insert(
            "/plaintext",
            get(fn_service(async |ctx: Ctx| ctx.into_parts().0.text_response())),
        )
        .insert(
            "/json",
            get(fn_service(async |ctx: Ctx| {
                let (req, state) = ctx.into_parts();
                req.json_response(state, &Message::new())
            })),
        )
        .insert(
            "/db",
            get(fn_service(async |ctx: Ctx| {
                let (req, state) = ctx.into_parts();
                let world = state.client.get_world().await?;
                req.json_response(state, &world)
            })),
        )
        .insert(
            "/fortunes",
            get(fn_service(async |ctx: Ctx| {
                let (req, state) = ctx.into_parts();
                let fortunes = state.client.tell_fortune().await?.render_once()?;
                req.html_response(fortunes)
            })),
        )
        .insert(
            "/queries",
            get(fn_service(async |ctx: Ctx| {
                let (req, state) = ctx.into_parts();
                let num = req.uri().query().parse_query();
                let worlds = state.client.get_worlds(num).await?;
                req.json_response(state, &worlds)
            })),
        )
        .insert(
            "/updates",
            get(fn_service(async |ctx: Ctx| {
                let (req, state) = ctx.into_parts();
                let num = req.uri().query().parse_query();
                let worlds = state.client.update(num).await?;
                req.json_response(state, &worlds)
            })),
        )
        .enclosed(ContextBuilder::new(|| async { db::create().await.map(State::new) }))
        .enclosed_fn(async |service, req| {
            let mut res = service.call(req).await.unwrap_or_else(error_handler);
            res.headers_mut().insert(SERVER, SERVER_HEADER_VALUE);
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
    error_response(match e {
        RouterError::Match(_) => StatusCode::NOT_FOUND,
        RouterError::NotAllowed(_) => StatusCode::METHOD_NOT_ALLOWED,
        RouterError::Service(_) => StatusCode::INTERNAL_SERVER_ERROR,
    })
}
