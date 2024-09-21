// reference of if/how moving from epoll to io-uring(or mixture of the two) make sense for network io.
// with comment on explaining why some practice are unrealistic

// custom global memory allocation don't affect real world performance in noticeable amount.
// in real world they should be used for reason like security, debug/profiling capability etc.
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
use xitca_service::{fn_service, Service, ServiceExt};

use self::{
    ser::{error_response, IntoResponse, Message, Request},
    util::{context_mw, Ctx, QueryParse, SERVER_HEADER_VALUE},
};

fn main() -> io::Result<()> {
    let addr = "0.0.0.0:8080".parse().unwrap();

    let cores = std::thread::available_parallelism().map(|num| num.get()).unwrap_or(56);

    let handle = core::iter::repeat_with(|| {
        std::thread::spawn(move || {
            tokio_uring::start(async {
                let socket = tokio::net::TcpSocket::new_v4()?;
                socket.set_reuseaddr(true)?;
                // unrealistic due to following reason:
                // 1. this only works good on unix system.
                // 2. no resource distribution adjustment between sockets on different threads. causing uneven workload
                // where some threads are idle while others busy. resulting in overall increased latency
                socket.set_reuseport(true)?;
                socket.bind(addr)?;
                let listener = socket.listen(1024)?;

                let service = fn_service(handler)
                    .enclosed(context_mw())
                    .enclosed(HttpServiceBuilder::h1().io_uring())
                    .call(())
                    .await
                    .unwrap();

                let service = std::rc::Rc::new(service);

                loop {
                    match listener.accept().await {
                        Ok((stream, addr)) => {
                            let stream = stream.into_std()?;
                            let stream = xitca_io::net::io_uring::TcpStream::from_std(stream);
                            let service = service.clone();
                            tokio::task::spawn_local(async move {
                                let _ = service.call((stream, addr)).await;
                            });
                        }
                        Err(e) => return Err(e),
                    };
                }
            })
        })
    })
    .take(cores)
    .collect::<Vec<_>>();

    // unrealistic due to no signal handling, not shutdown handling. when killing this process all resources that
    // need clean async shutdown will be leaked.
    for handle in handle {
        handle.join().unwrap()?;
    }

    Ok(())
}

async fn handler<B>(ctx: Ctx<'_, Request<B>>) -> Result<http::Response<ResponseBody>, Infallible> {
    let (req, state) = ctx.into_parts();
    // unrealistic due to lack of http method check
    let mut res = match req.uri().path() {
        // unrealistic due to lack of dynamic route matching.
        "/plaintext" => req.text_response().unwrap(),
        "/json" => req.json_response(state, &Message::new()).unwrap(),
        "/db" => {
            // unrealistic due to no error handling. any db/serialization error will cause process crash
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
