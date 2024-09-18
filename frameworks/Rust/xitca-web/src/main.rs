mod db;
mod ser;
mod util;

use xitca_http::{
    h1::RequestBody,
    http::{header::SERVER, StatusCode},
    util::service::{
        route::get,
        router::{Router, RouterError},
    },
    HttpServiceBuilder,
};
use xitca_service::{fn_service, Service, ServiceExt};

use ser::{error_response, IntoResponse, Message, Request, Response};
use util::{context_mw, HandleResult, QueryParse, SERVER_HEADER_VALUE};

type Ctx<'a> = util::Ctx<'a, Request<RequestBody>>;

fn main() -> std::io::Result<()> {
    let service = Router::new()
        .insert("/plaintext", get(fn_service(plain_text)))
        .insert("/json", get(fn_service(json)))
        .insert("/db", get(fn_service(db)))
        .insert("/fortunes", get(fn_service(fortunes)))
        .insert("/queries", get(fn_service(queries)))
        .insert("/updates", get(fn_service(updates)))
        .enclosed_fn(middleware)
        .enclosed(context_mw())
        .enclosed(HttpServiceBuilder::h1().io_uring());
    xitca_server::Builder::new()
        .bind("xitca-web", "0.0.0.0:8080", service)?
        .build()
        .wait()
}

async fn middleware<S>(service: &S, req: Ctx<'_>) -> Result<Response, core::convert::Infallible>
where
    S: for<'c> Service<Ctx<'c>, Response = Response, Error = RouterError<util::Error>>,
{
    let mut res = service.call(req).await.unwrap_or_else(|e| match e {
        RouterError::Match(_) => error_response(StatusCode::NOT_FOUND),
        RouterError::NotAllowed(_) => error_response(StatusCode::METHOD_NOT_ALLOWED),
        RouterError::Service(e) => {
            println!("{e}");
            error_response(StatusCode::INTERNAL_SERVER_ERROR)
        }
    });
    res.headers_mut().insert(SERVER, SERVER_HEADER_VALUE);
    Ok(res)
}

async fn plain_text(ctx: Ctx<'_>) -> HandleResult<Response> {
    ctx.into_parts().0.text_response()
}

async fn json(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    req.json_response(state, &Message::new())
}

async fn db(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    let world = state.client.get_world().await?;
    req.json_response(state, &world)
}

async fn fortunes(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    use sailfish::TemplateOnce;
    let fortunes = state.client.tell_fortune().await?.render_once()?;
    req.html_response(fortunes)
}

async fn queries(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    let num = req.uri().query().parse_query();
    let worlds = state.client.get_worlds(num).await?;
    req.json_response(state, &worlds)
}

async fn updates(ctx: Ctx<'_>) -> HandleResult<Response> {
    let (req, state) = ctx.into_parts();
    let num = req.uri().query().parse_query();
    let worlds = state.client.update(num).await?;
    req.json_response(state, &worlds)
}
