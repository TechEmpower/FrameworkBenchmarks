use std::error::Error;
use std::io;
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;

use hyper::server::conn::http1::Builder;
use hyper_util::rt::TokioIo;
use tokio::net::{TcpListener, TcpSocket};
use viz::{Responder, Router, Tree};

pub async fn serve(router: Router) -> Result<(), Box<dyn Error + Send + Sync>> {
    let tree = Arc::<Tree>::new(router.into());
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8080));
    let listener = reuse_listener(addr).expect("couldn't bind to addr");

    println!("Started viz server at 8080");

    loop {
        let (tcp, _) = listener.accept().await?;
        let io = TokioIo::new(tcp);
        let tree = tree.clone();

        tokio::task::spawn(async move {
            Builder::new()
                .pipeline_flush(true)
                .serve_connection(io, Responder::<Arc<SocketAddr>>::new(tree, None))
                .with_upgrades()
                .await
        });
    }
}

fn reuse_listener(addr: SocketAddr) -> io::Result<TcpListener> {
    let socket = match addr {
        SocketAddr::V4(_) => TcpSocket::new_v4()?,
        SocketAddr::V6(_) => TcpSocket::new_v6()?,
    };

    #[cfg(unix)]
    {
        if let Err(e) = socket.set_reuseport(true) {
            eprintln!("error setting SO_REUSEPORT: {e}");
        }
    }

    socket.set_reuseaddr(true)?;
    socket.bind(addr)?;
    socket.listen(1024)
}
