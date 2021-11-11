use std::net::SocketAddr;

use futures::{Future, Stream};
use tokio_core::net::TcpStream;
use tokio_core::reactor::Handle;
use tokio_postgres::{Client, Config, NoTls, Statement};

pub struct Db {
    client: Client,
    fortune: Statement,
}

pub struct Fortune {
    pub id: i32,
    pub message: String,
}

pub fn connect(
    addr: SocketAddr,
    config: Config,
    handle: Handle,
) -> impl Future<Item = Db, Error = ()> {
    TcpStream::connect(&addr, &handle)
        .map_err(|e| panic!("error connecting to postgresql: {}", e))
        .and_then(move |tcp| {
            config
                .connect_raw(tcp, NoTls)
                .map_err(|e| panic!("error connecting to postgresql: {}", e))
        })
        .and_then(move |(mut client, conn)| {
            handle.spawn(conn.map_err(|e| panic!("postgres connection error: {}", e)));

            client
                .prepare("SELECT id, message FROM fortune")
                .map_err(|_| ())
                .map(move |fortune| Db { client, fortune })
        })
}

impl Db {
    pub fn tell_fortune(
        &mut self,
    ) -> impl Future<Item = Vec<Fortune>, Error = ::tokio_postgres::Error> {
        let items = vec![Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        }];

        self.client
            .query(&self.fortune, &[])
            .fold(items, move |mut items, row| {
                items.push(Fortune {
                    id: row.get(0),
                    message: row.get(1),
                });
                Ok(items)
            })
            .map(|mut items| {
                items.sort_by(|it, next| it.message.cmp(&next.message));
                items
            })
    }
}
