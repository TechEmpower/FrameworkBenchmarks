// unrealistic bench showcase popular tricks for boosting bench score artificially

// custom global memory allocator don't affect real world performance in noticeable amount.
// in real world they should be used for reason like security, debug/profiling capability etc.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[path = "db_unrealistic.rs"]
mod db;
mod ser;
mod util;

use std::io;

use xitca_http::{
    bytes::BufMut,
    h1::dispatcher_unreal::{Dispatcher, Request, Response},
    http::StatusCode,
};
use xitca_service::Service;

use self::{
    ser::Message,
    util::{QueryParse, State},
};

fn main() -> io::Result<()> {
    let addr = "0.0.0.0:8080".parse().unwrap();

    let cores = std::thread::available_parallelism().map(|num| num.get()).unwrap_or(56);

    let mut ids = core_affinity::get_core_ids().unwrap();

    let worker = move |id: Option<core_affinity::CoreId>| {
        if let Some(id) = id {
            let _ = core_affinity::set_for_current(id);
        }

        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build_local(Default::default())
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
                            let service = service.clone();
                            tokio::task::spawn_local(async move {
                                let _ = service.call(stream.into()).await;
                            });
                        }
                        Err(e) => return Err(e),
                    };
                }
            })
    };

    let handle = core::iter::repeat_with(|| {
        let id = ids.pop();
        std::thread::spawn(move || worker(id))
    })
    .take(cores - 1)
    .collect::<Vec<_>>();

    // unrealistic due to no signal handling, not shutdown handling. when killing this process all resources that
    // need clean async shutdown will be leaked.
    worker(ids.pop())?;
    for handle in handle {
        handle.join().unwrap()?;
    }

    Ok(())
}

async fn handler<'h>(req: Request<'h, State<db::Client>>, res: Response<'h>) -> Response<'h, 3> {
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
                .body_writer(|buf| buf.extend_from_slice(b"Hello, World!"))
        }
        "/json" => res
            .status(StatusCode::OK)
            .header("content-type", "application/json")
            .header("server", "X")
            // unrealistic content length header.
            .header("content-length", "27")
            // snoic-rs crate is realistic approach to json serializer.
            // That said xitca-web by default utilize serde-json as serializer making it an unrealistic representation of framework performance
            .body_writer(|buf| sonic_rs::to_writer(buf.writer(), &Message::new()).unwrap()),

        // all database related categories are unrealistic. please reference db_unrealistic module for detail.
        "/fortunes" => {
            let fortunes = req.ctx.client.tell_fortune().await.unwrap().render_once().unwrap();
            res.status(StatusCode::OK)
                .header("content-type", "text/html; charset=utf-8")
                .header("server", "X")
                .body(fortunes.as_bytes())
        }
        "/db" => {
            // unrealistic due to no error handling. any db/serialization error will cause process crash.
            // the same goes for all following unwraps on database related functions.
            let world = req.ctx.client.get_world().await.unwrap();
            json_response(res, req.ctx, &world)
        }
        p if p.starts_with("/q") => {
            let num = p["/queries?q=".len()..].parse_query();
            let worlds = req.ctx.client.get_worlds(num).await.unwrap();
            json_response(res, req.ctx, &worlds)
        }
        p if p.starts_with("/u") => {
            let num = p["/updates?q=".len()..].parse_query();
            let worlds = req.ctx.client.update(num).await.unwrap();
            json_response(res, req.ctx, &worlds)
        }
        _ => res.status(StatusCode::NOT_FOUND).header("server", "X").body(&[]),
    }
}

fn json_response<'r, DB, T>(res: Response<'r>, state: &State<DB>, val: &T) -> Response<'r, 3>
where
    T: serde_core::Serialize,
{
    let buf = &mut *state.write_buf.borrow_mut();
    sonic_rs::to_writer(buf.writer(), val).unwrap();
    let res = res
        .status(StatusCode::OK)
        .header("content-type", "application/json")
        .header("server", "X")
        .body(buf.as_ref());
    buf.clear();
    res
}
