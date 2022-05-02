use std::io;
use std::net::{Ipv4Addr, SocketAddr};

use hyper::server::conn::AddrIncoming;
use tokio::net::{TcpListener, TcpSocket};

pub fn builder() -> hyper::server::Builder<AddrIncoming> {
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8000));
    let listener = create_listener(addr).expect("couldn't bind to addr");
    let incoming = AddrIncoming::from_listener(listener).unwrap();

    println!("Started axum server at 8000 ");

    axum::Server::builder(incoming)
        .http1_only(true)
        .tcp_nodelay(true)
}

fn create_listener(addr: SocketAddr) -> io::Result<TcpListener> {
    let socket = match addr {
        SocketAddr::V4(_) => TcpSocket::new_v4()?,
        SocketAddr::V6(_) => TcpSocket::new_v6()?,
    };

    socket.bind(addr)?;
    socket.listen(1024)
}
