// unrealistic bench showcase popular tricks for boosting bench score artificially

// custom global memory allocator don't affect real world performance in noticeable amount.
// in real world they should be used for reason like security, debug/profiling capability etc.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod db;
mod db_unrealistic;
mod ser;
mod util;

use std::io;

use xitca_http::{
    h1::dispatcher_unreal::{Dispatcher, Request, Response},
    http::StatusCode,
};
use xitca_service::Service;

use self::{
    ser::{HELLO, Message},
    util::QueryParse,
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

                let client = db_unrealistic::Client::create().await.unwrap();

                // unrealistic http dispatcher. no spec check. no security feature.
                let service = Dispatcher::new(handler, client);

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
        let _ = handle.join().unwrap();
    }

    Ok(())
}

async fn handler<'h>(req: Request<'h, db_unrealistic::Client>, res: Response<'h>) -> Response<'h, 3> {
    // unrealistic due to no http method check
    match req.path {
        // unrealistic due to no dynamic path matching
        "/plaintext" => {
            // unrealistic due to no body streaming and no post processing. violating middleware feature of xitca-web
            res.status(StatusCode::OK)
                .header("content-type", "text/plain")
                .header("server", "X")
                .body(HELLO.as_bytes())
        }
        "/json" => json_response(res, Message::HELLO),

        // all database related categories are unrealistic. please reference db_unrealistic module for detail.
        "/fortunes" => {
            let fortunes = req.ctx.fortunes().await.unwrap().render_once().unwrap();
            res.status(StatusCode::OK)
                .header("content-type", "text/html; charset=utf-8")
                .header("server", "X")
                .body(fortunes.as_bytes())
        }
        "/db" => {
            // unrealistic due to no error handling. any db/serialization error will cause process crash.
            // the same goes for all following unwraps on database related functions.
            let world = req.ctx.db().await.unwrap();
            json_response(res, &world)
        }
        p if p.starts_with("/q") => {
            let num = p["/queries?q=".len()..].parse_query();
            let worlds = req.ctx.queries(num).await.unwrap();
            json_response(res, &worlds)
        }
        p if p.starts_with("/u") => {
            let num = p["/updates?q=".len()..].parse_query();
            let worlds = req.ctx.updates(num).await.unwrap();
            json_response(res, &worlds)
        }
        _ => res.status(StatusCode::NOT_FOUND).header("server", "X").body(&[]),
    }
}

fn json_response<'r>(res: Response<'r>, val: &impl serde_core::Serialize) -> Response<'r, 3> {
    // snoic-rs crate is realistic approach to json serializer.
    // That said xitca-web by default utilize serde-json as serializer making it an unrealistic representation of framework performance
    let buf = ser::json_serialize(val).unwrap();
    res.status(StatusCode::OK)
        .header("content-type", "application/json")
        .header("server", "X")
        .body(buf.as_ref())
}
