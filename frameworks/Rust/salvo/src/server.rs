use std::io;
use std::net::SocketAddr;

use salvo::hyper::server::conn::AddrIncoming;
use salvo::hyper;
use socket2::{Domain, Socket, Type};
use tokio::net::TcpListener;

pub fn builder() -> hyper::server::Builder<AddrIncoming> {
    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    let listener = reuse_listener(addr).expect("couldn't bind to addr");
    let incoming = AddrIncoming::from_listener(listener).unwrap();
    hyper::Server::builder(incoming).http1_only(true).tcp_nodelay(true)
}

fn reuse_listener(addr: SocketAddr) -> io::Result<TcpListener> {
    let socket = match addr {
        SocketAddr::V4(_) => Socket::new(Domain::IPV4, Type::STREAM, None)?,
        SocketAddr::V6(_) => Socket::new(Domain::IPV6, Type::STREAM, None)?,
    };

    #[cfg(unix)]
    {
        if let Err(e) = socket.set_reuse_port(true) {
            eprintln!("error setting SO_REUSEPORT: {}", e);
        }
    }

    socket.set_reuse_address(true)?;
    socket.set_nonblocking(true)?;
    socket.set_nodelay(true)?;
    socket.bind(&addr.into())?;
    socket.listen(1024)?;
    Ok(TcpListener::from_std(socket.into())?)
}
