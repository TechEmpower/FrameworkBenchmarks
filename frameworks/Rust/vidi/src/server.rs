use std::error::Error;
use std::future::Future;
use std::io;
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;
use std::thread;

use hyper::server::conn::http1::Builder;
use hyper_util::rt::TokioIo;
use socket2::{Domain, SockAddr, Socket};
use tokio::{net::TcpListener, runtime};
use vidi::header::{HeaderValue, SERVER};
use vidi::{HandlerExt, Responder, Response, Router, Tree};

pub async fn serve(router: Router) -> Result<(), Box<dyn Error + Send + Sync>> {
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8080));
    let socket = create_socket(addr).expect("couldn't bind to addr");
    let listener = TcpListener::from_std(socket.into())?;

    let router = router.map_handler(|h| {
        h.map(|mut res: Response| {
            let headers = res.headers_mut();
            headers.insert(SERVER, HeaderValue::from_static("Vidi"));
            res
        })
        .boxed()
    });
    let tree = Arc::<Tree>::new(router.into());

    let mut http = Builder::new();
    http.pipeline_flush(true);

    println!("Started vidi server at 8080");

    loop {
        let (tcp, _) = listener.accept().await?;
        tcp.set_nodelay(true).expect("couldn't set TCP_NODELAY!");

        let http = http.clone();
        let tree = tree.clone();

        tokio::spawn(async move {
            http.serve_connection(
                TokioIo::new(tcp),
                Responder::<Arc<SocketAddr>>::new(tree, None),
            )
            .await
        });
    }
}

fn create_socket(addr: SocketAddr) -> Result<Socket, io::Error> {
    let domain = match addr {
        SocketAddr::V4(_) => Domain::IPV4,
        SocketAddr::V6(_) => Domain::IPV6,
    };
    let addr = SockAddr::from(addr);
    let socket = Socket::new(domain, socket2::Type::STREAM, None)?;
    let backlog = 4096;
    #[cfg(unix)]
    socket.set_reuse_port(true)?;
    socket.set_reuse_address(true)?;
    socket.set_tcp_nodelay(true)?;
    socket.set_nonblocking(true)?;
    socket.bind(&addr)?;
    socket.listen(backlog)?;

    Ok(socket)
}

pub fn run<Fut>(f: fn() -> Fut)
where
    Fut: Future + Send + 'static,
{
    for _ in 1..num_cpus::get() {
        let runtime = runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        thread::spawn(move || {
            runtime.block_on(f());
        });
    }

    let runtime = runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    runtime.block_on(f());
}
