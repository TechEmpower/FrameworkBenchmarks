#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::cmp;
use std::collections::HashMap;
use std::fmt::Write;
use std::io;
use std::thread::available_parallelism;

use anyhow::Error;
use async_trait::async_trait;
use futures::stream::futures_unordered::FuturesUnordered;
use futures::TryStreamExt;
use once_cell::sync::OnceCell;
use rand::distributions::{Distribution, Uniform};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use salvo::routing::FlowCtrl;
use tokio_postgres::types::ToSql;
use tokio_postgres::{self, Client, NoTls, Statement};

mod models;
mod server;
use models::*;

const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
static CACHED_WORLDS: OnceCell<Vec<World>> = OnceCell::new();
type DbResult<T> = Result<T, tokio_postgres::Error>;

struct PgConnection {
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
struct WorldHandler {
    conn: PgConnection,
}
impl WorldHandler {
    async fn new() -> Self {
        Self {
            conn: PgConnection::create(DB_URL)
                .await.unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)),
        }
    }
}
#[async_trait]
impl Handler for WorldHandler {
    async fn handle(&self, _req: &mut Request, _depot: &mut Depot, res: &mut Response, _ctrl: &mut FlowCtrl) {
        res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
        let world = self.conn.get_world().await.unwrap();
        res.render(Json(world));
    }
}
struct WorldsHandler {
    conn: PgConnection,
}
impl WorldsHandler {
    async fn new() -> Self {
        Self {
            conn: PgConnection::create(DB_URL)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)),
        }
    }
}
#[async_trait]
impl Handler for WorldsHandler {
    async fn handle(&self, req: &mut Request, _depot: &mut Depot, res: &mut Response, _ctrl: &mut FlowCtrl) {
        let count = req.query::<u16>("q").unwrap_or(1);
        let count = cmp::min(500, cmp::max(1, count));
        res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
        let worlds = self.conn.get_worlds(count).await.unwrap();
        res.render(Json(worlds));
    }
}
struct UpdatesHandler {
    conn: PgConnection,
}
impl UpdatesHandler {
    async fn new() -> Self {
        Self {
            conn: PgConnection::create(DB_URL)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)),
        }
    }
}
#[async_trait]
impl Handler for UpdatesHandler {
    async fn handle(&self, req: &mut Request, _depot: &mut Depot, res: &mut Response, _ctrl: &mut FlowCtrl) {
        let count = req.query::<u16>("q").unwrap_or(1);
        let count = cmp::min(500, cmp::max(1, count));
        res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
        let worlds = self.conn.update(count).await.unwrap();
        res.render(Json(worlds));
    }
}
struct FortunesHandler {
    conn: PgConnection,
}
impl FortunesHandler {
    async fn new() -> Self {
        Self {
            conn: PgConnection::create(DB_URL)
                .await
                .unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)),
        }
    }
}
#[async_trait]
impl Handler for FortunesHandler {
    async fn handle(&self, _req: &mut Request, _depot: &mut Depot, res: &mut Response, _ctrl: &mut FlowCtrl) {
        let mut body = String::new();
        write!(&mut body, "{}", self.conn.tell_fortune().await.unwrap()).unwrap();
        res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
        res.render(Text::Html(body));
    }
}

#[handler]
async fn cached_queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let mut worlds = Vec::with_capacity(count);
    let mut rng = SmallRng::from_entropy();
    for _ in 0..count {
        let idx = rng.gen_range(0..10_000);
        unsafe {
            let w = CACHED_WORLDS.get_unchecked().get(idx).unwrap();
            worlds.push(w);
        }
    }
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Json(worlds));
    Ok(())
}

async fn populate_cache() -> Result<(), Error> {
    let conn = PgConnection::create(DB_URL).await?;
    let worlds = conn.get_worlds(10_000).await?;
    CACHED_WORLDS.set(worlds).unwrap();
    Ok(())
}

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(async {
        populate_cache().await.expect("error cache worlds");
    });
    for _ in 1..available_parallelism().map(|n| n.get()).unwrap_or(16) {
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve());
        });
    }
    println!("Started http server: 127.0.0.1:8080");
    rt.block_on(serve());
}

async fn serve() {
    let router = Router::new()
        .push(Router::with_path("db").get(WorldHandler::new().await))
        .push(Router::with_path("fortunes").get(FortunesHandler::new().await))
        .push(Router::with_path("queries").get(WorldsHandler::new().await))
        .push(Router::with_path("cached_queries").get(cached_queries))
        .push(Router::with_path("updates").get(UpdatesHandler::new().await));

    server::builder().serve(Service::new(router)).await.unwrap();
}
