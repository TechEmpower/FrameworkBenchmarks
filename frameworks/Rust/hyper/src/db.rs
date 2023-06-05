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

pub async fn connect(
    addr: SocketAddr,
    config: Config,
    handle: Handle,
) -> Result<Db, Box<dyn std::error::Error>> {
    let stream = TcpStream::connect(&addr).await?;
    let (client, conn) = config.connect_raw(stream, NoTls).await?;
    handle.spawn(conn);
    let fortune = client.prepare("SELECT id, message FROM fortune").await?;
    Ok(Db { client, fortune })
}

impl Db {
    pub async fn tell_fortune(&self) -> Result<Vec<Fortune>, tokio_postgres::Error> {
        let mut items = vec![Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        }];

        let rows = self.client.query(&self.fortune, &[]).await?;
        for row in rows {
            items.push(Fortune {
                id: row.get(0),
                message: row.get(1),
            });
        }
        items.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(items)
    }
}
