use std::io;
use std::net::{Ipv4Addr, SocketAddr};

use salvo::hyper::server::conn::AddrIncoming;
use salvo::hyper;
use tokio::net::{TcpListener, TcpSocket};

pub fn builder() -> hyper::server::Builder<AddrIncoming> {
    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8080));
    let listener = reuse_listener(addr).expect("couldn't bind to addr");
    let incoming = AddrIncoming::from_listener(listener).unwrap();
    hyper::Server::builder(incoming).http1_only(true).tcp_nodelay(true)
}

fn reuse_listener(addr: SocketAddr) -> io::Result<TcpListener> {
    let socket = match addr {
        SocketAddr::V4(_) => TcpSocket::new_v4()?,
        SocketAddr::V6(_) => TcpSocket::new_v6()?,
    };

    #[cfg(unix)]
        {
            if let Err(e) = socket.set_reuseport(true) {
                eprintln!("error setting SO_REUSEPORT: {}", e);
            }
        }

    socket.set_reuseaddr(true)?;
    socket.bind(addr)?;
    socket.listen(1024)
}