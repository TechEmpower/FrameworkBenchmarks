use std::{
    io,
    net::{Ipv4Addr, SocketAddr},
};

use hyper::server::conn::AddrIncoming;
use tokio::net::{TcpListener, TcpSocket};

/// Build a new `hyper` server that listens for connections on port 8000.
pub fn server_builder() -> hyper::server::Builder<AddrIncoming> {
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8000));
    let listener = reuse_listener(addr).expect("couldn't bind to addr");
    let incoming = AddrIncoming::from_listener(listener).unwrap();

    println!("Started hyper server at 8000");

    hyper::Server::builder(incoming)
        .http1_only(true)
        .tcp_nodelay(true)
}

/// Create a new `TcpListener` that can be reused by multiple processes (if supported by the OS).
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
