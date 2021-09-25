#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::cmp;
use std::collections::HashMap;
use std::fmt::Write;
use std::io;

use anyhow::Error;
use futures::stream::futures_unordered::FuturesUnordered;
use futures::TryStreamExt;
use once_cell::sync::OnceCell;
use rand::distributions::{Distribution, Uniform};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use tokio_postgres::types::ToSql;
use tokio_postgres::{self, Client, NoTls, Statement};

mod models;
mod server;
use models::*;

const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
pub static DB_CONN: OnceCell<PgConnection> = OnceCell::new();
type DbResult<T> = Result<T, tokio_postgres::Error>;
pub fn connect() -> &'static PgConnection {
    unsafe { DB_CONN.get_unchecked() }
}
pub struct PgConnection {
    client: Client,
    fortune: Statement,
    world: Statement,
    updates: HashMap<u16, Statement>,
}

impl PgConnection {
    pub async fn create(db_url: &str) -> Result<PgConnection, io::Error> {
        let (client, conn) = tokio_postgres::connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");
        tokio::spawn(async move {
            if let Err(e) = conn.await {
                eprintln!("connection error: {}", e);
            }
        });

        let fortune = client.prepare("SELECT id, message FROM fortune").await.unwrap();
        let world = client.prepare("SELECT * FROM world WHERE id=$1").await.unwrap();
        let mut updates = HashMap::new();
        for num in 1..=500u16 {
            let mut pl: u16 = 1;
            let mut q = String::new();
            q.push_str("UPDATE world SET randomnumber = CASE id ");
            for _ in 1..=num {
                let _ = write!(&mut q, "when ${} then ${} ", pl, pl + 1);
                pl += 2;
            }
            q.push_str("ELSE randomnumber END WHERE id IN (");
            for _ in 1..=num {
                let _ = write!(&mut q, "${},", pl);
                pl += 1;
            }
            q.pop();
            q.push(')');
            updates.insert(num, client.prepare(&q).await.unwrap());
        }

        Ok(PgConnection {
            client,
            fortune,
            world,
            updates,
        })
    }

    async fn query_one_world(&self, w_id: i32) -> DbResult<World> {
        self.client.query_one(&self.world, &[&w_id]).await.map(|row| World {
            id: row.get(0),
            randomnumber: row.get(1),
        })
    }

    pub async fn get_world(&self) -> DbResult<World> {
        let mut rng = SmallRng::from_entropy();
        let id: i32 = rng.gen_range(1..10_001);
        self.query_one_world(id).await
    }
    pub async fn get_worlds(&self, count: u16) -> DbResult<Vec<World>> {
        let worlds = {
            let mut rng = SmallRng::from_entropy();
            let between = Uniform::from(1..10_001);
            (0..count)
                .map(|_| {
                    let id: i32 = between.sample(&mut rng);
                    self.query_one_world(id)
                })
                .collect::<FuturesUnordered<_>>()
        };

        worlds.try_collect().await
    }

    pub async fn update(&self, count: u16) -> DbResult<Vec<World>> {
        let worlds = {
            let mut rng = SmallRng::from_entropy();
            let between = Uniform::from(1..10_001);
            (0..count)
                .map(|_| {
                    let id: i32 = between.sample(&mut rng);
                    let w_id: i32 = between.sample(&mut rng);
                    async move {
                        let mut world = self.query_one_world(w_id).await?;
                        world.randomnumber = id;
                        Ok(world)
                    }
                })
                .collect::<FuturesUnordered<_>>()
        };

        let worlds = worlds.try_collect::<Vec<_>>().await?;
        let mut params = Vec::<&(dyn ToSql + Sync)>::with_capacity(count as usize * 3);
        for w in &worlds {
            params.push(&w.id);
            params.push(&w.randomnumber);
        }
        for w in &worlds {
            params.push(&w.id);
        }

        let st = self.updates.get(&count).unwrap();
        self.client.query(st, params.as_slice()).await?;
        Ok(worlds)
    }

    pub async fn tell_fortune(&self) -> DbResult<FortunesTemplate> {
        let mut items = self
            .client
            .query(&self.fortune, &[])
            .await?
            .iter()
            .map(|row| Fortune {
                id: row.get(0),
                message: row.get(1),
            })
            .collect::<Vec<_>>();
        items.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });
        items.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(FortunesTemplate { items })
    }
}

markup::define! {
    FortunesTemplate(items: Vec<Fortune>) {
        {markup::doctype()}
        html {
            head {
                title { "Fortunes" }
            }
            body {
                table {
                    tr { th { "id" } th { "message" } }
                    @for item in items {
                        tr {
                            td { {item.id} }
                            td { {markup::raw(v_htmlescape::escape(&item.message).to_string())} }
                        }
                    }
                }
            }
        }
    }
}

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(async {
        let conn = PgConnection::create(DB_URL)
            .await
            .expect(&format!("Error connecting to {}", &DB_URL));
        DB_CONN.set(conn).ok();
    });
    for _ in 1..num_cpus::get() {
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve());
        });
    }
    rt.block_on(serve());
}

async fn serve() {
    println!("Started http server: 127.0.0.1:8080");

    #[fn_handler]
    async fn world_row(_req: &mut Request, res: &mut Response) -> Result<(), Error> {
        res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
        let conn = connect();
        let world = conn.get_world().await?;
        res.render_json(&world);
        Ok(())
    }

    #[fn_handler]
    async fn queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
        let count = req.get_query::<u16>("q").unwrap_or(1);
        let count = cmp::min(500, cmp::max(1, count));
        res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
        let conn = connect();
        let worlds = conn.get_worlds(count).await?;
        res.render_json(&worlds);
        Ok(())
    }

    #[fn_handler]
    async fn updates(req: &mut Request, res: &mut Response) -> Result<(), Error> {
        let count = req.get_query::<u16>("q").unwrap_or(1);
        let count = cmp::min(500, cmp::max(1, count));

        res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
        let conn = connect();
        let worlds = conn.update(count).await?;
        res.render_json(&worlds);
        Ok(())
    }

    #[fn_handler]
    async fn fortunes(_req: &mut Request, res: &mut Response) -> Result<(), Error> {
        let conn = connect();
        let mut body = String::new();
        write!(&mut body, "{}", conn.tell_fortune().await?).unwrap();

        res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
        res.render_html_text(&body);
        Ok(())
    }

    let router = Router::new()
        .push(Router::new().path("db").get(world_row))
        .push(Router::new().path("fortunes").get(fortunes))
        .push(Router::new().path("queries").get(queries))
        .push(Router::new().path("updates").get(updates));

    server::builder().serve(Service::new(router)).await.unwrap();
}
