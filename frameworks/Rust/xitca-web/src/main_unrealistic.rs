// unrealistic bench showcase popular tricks for boosting bench score artificially

// custom global memory allocator don't affect real world performance in noticeable amount.
// in real world they should be used for reason like security, debug/profiling capability etc.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[path = "db_unrealistic.rs"]
mod db;
mod ser;
mod util;

use std::{convert::Infallible, io};

use xitca_http::{
    bytes::BufMutWriter,
    h1::dispatcher_unreal::{Dispatcher, Request, Response},
    http::StatusCode,
};
use xitca_io::net::TcpStream;
use xitca_service::Service;

use self::{
    ser::Message,
    util::{QueryParse, State},
};

fn main() -> io::Result<()> {
    let addr = "0.0.0.0:8080".parse().unwrap();

    let cores = std::thread::available_parallelism().map(|num| num.get()).unwrap_or(56);

    let handle = core::iter::repeat_with(|| {
        std::thread::spawn(move || {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build_local(&Default::default())
                .unwrap()
                .block_on(async {
                    let socket = tokio::net::TcpSocket::new_v4()?;
                    socket.set_reuseaddr(true)?;
                    // unrealistic due to following reason:
                    // 1. this only works good on unix system.
                    // 2. no resource distribution adjustment between sockets on different threads. causing uneven workload
                    // where some threads are idle while others busy. resulting in overall increased latency
                    socket.set_reuseport(true)?;
                    socket.bind(addr)?;
                    let listener = socket.listen(1024)?;

                    let client = db::create().await.unwrap();

                    // unrealistic http dispatcher. no spec check. no security feature.
                    let service = Dispatcher::new(handler, State::new(client));

                    loop {
                        match listener.accept().await {
                            Ok((stream, _)) => {
                                let stream = stream.into_std()?;
                                let stream = TcpStream::from_std(stream)?;
                                let service = service.clone();
                                tokio::task::spawn_local(async move {
                                    let _ = service.call(stream).await;
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

async fn handler<'h>(req: Request<'h>, res: Response<'h>, state: &State<db::Client>) -> Response<'h, 3> {
    // unrealistic due to no http method check
    match req.path {
        // unrealistic due to no dynamic path matching
        "/plaintext" => {
            // unrealistic due to no body streaming and no post processing. violating middleware feature of xitca-web
            res.status(StatusCode::OK)
                .header("content-type", "text/plain")
                .header("server", "X")
                // unrealistic content length header.
                .header("content-length", "13")
                .body_writer(|buf| Ok::<_, Infallible>(buf.extend_from_slice(b"Hello, World!")))
                .unwrap()
        }
        "/json" => res
            .status(StatusCode::OK)
            .header("content-type", "application/json")
            .header("server", "X")
            // unrealistic content length header.
            .header("content-length", "27")
            .body_writer(|buf| serde_json::to_writer(BufMutWriter(buf), &Message::new()))
            .unwrap(),
        // all database related categories are unrealistic. please reference db_unrealistic module for detail.
        "/fortunes" => {
            use sailfish::TemplateOnce;
            let fortunes = state.client.tell_fortune().await.unwrap().render_once().unwrap();
            res.status(StatusCode::OK)
                .header("content-type", "text/html; charset=utf-8")
                .header("server", "X")
                .body(fortunes.as_bytes())
        }
        "/db" => {
            // unrealistic due to no error handling. any db/serialization error will cause process crash.
            // the same goes for all following unwraps on database related functions.
            let world = state.client.get_world().await.unwrap();
            json_response(res, state, &world)
        }
        p if p.starts_with("/q") => {
            let num = p["/queries?q=".len()..].parse_query();
            let worlds = state.client.get_worlds(num).await.unwrap();
            json_response(res, state, &worlds)
        }
        p if p.starts_with("/u") => {
            let num = p["/updates?q=".len()..].parse_query();
            let worlds = state.client.update(num).await.unwrap();
            json_response(res, state, &worlds)
        }
        _ => res.status(StatusCode::NOT_FOUND).header("server", "X").body(&[]),
    }
}

fn json_response<'r, DB, T>(res: Response<'r>, state: &State<DB>, val: &T) -> Response<'r, 3>
where
    T: serde::Serialize,
{
    let buf = &mut *state.write_buf.borrow_mut();
    serde_json::to_writer(BufMutWriter(buf), val).unwrap();
    let res = res
        .status(StatusCode::OK)
        .header("content-type", "application/json")
        .header("server", "X")
        .body(buf.as_ref());
    buf.clear();
    res
}
