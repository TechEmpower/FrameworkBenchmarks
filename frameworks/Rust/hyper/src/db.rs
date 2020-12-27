use futures::TryFutureExt;
use std::net::SocketAddr;
use tokio::net::TcpStream;
use tokio::runtime::Handle;
use tokio_postgres::{Client, Config, NoTls, Statement};

pub struct Db {
    client: Client,
    fortune: Statement,
}

pub struct Fortune {
    pub id: i32,
    pub message: String,
}

pub async fn connect(addr: SocketAddr, config: Config, handle: Handle) -> Db {
    match TcpStream::connect(&addr).await {
        Err(e) => panic!("error connecting to postgresql: {}", e),
        Ok(tcp) => match config.connect_raw(tcp, NoTls).await {
            Err(e) => panic!("error connecting to postgresql: {}", e),
            Ok((client, conn)) => {
                handle.spawn(conn.map_err(|e| panic!("postgres connection error: {}", e)));

                let fortune = client
                    .prepare("SELECT id, message FROM fortune")
                    .await
                    .unwrap();
                Db { client, fortune }
            }
        },
    }
}

impl Db {
    pub async fn tell_fortune(&mut self) -> Result<Vec<Fortune>, ::tokio_postgres::Error> {
        let items = vec![Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        }];

        let query = self.client.query(&self.fortune, &[]).await?;

        let mut items = query.iter().fold(items, move |mut items, row| {
            items.push(Fortune {
                id: row.get(0),
                message: row.get(1),
            });
            items
        });
        items.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(items)
    }
}
