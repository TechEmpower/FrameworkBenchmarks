#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fmt::Write;
use std::io;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use may_minihttp::{BodyWriter, HttpService, HttpServiceFactory, Request, Response};
use may_postgres::{self, Client, RowStream, Statement};
use oorandom::Rand32;
use serde::Serialize;
use smallvec::SmallVec;

mod utils {
    use may_postgres::types::ToSql;
    use std::cmp;

    pub fn get_query_param(query: &str) -> u16 {
        let q = if let Some(pos) = query.find("?q") {
            query.split_at(pos + 3).1.parse::<u16>().ok().unwrap_or(1)
        } else {
            1
        };
        cmp::min(500, cmp::max(1, q))
    }

    pub fn slice_iter<'a>(
        s: &'a [&'a (dyn ToSql + Sync)],
    ) -> impl ExactSizeIterator<Item = &'a dyn ToSql> + 'a {
        s.iter().map(|s| *s as _)
    }
}

#[derive(Serialize)]
struct HeloMessage {
    message: &'static str,
}

#[derive(Serialize)]
struct WorldRow {
    id: i32,
    randomnumber: i32,
}

#[derive(Serialize)]
pub struct Fortune {
    id: i32,
    message: String,
}

markup::define! {
    FortunesTemplate(fortunes: Vec<Fortune>) {
        {markup::doctype()}
        html {
            head {
                title { "Fortunes" }
            }
            body {
                table {
                    tr { th { "id" } th { "message" } }
                    @for item in {fortunes} {
                        tr {
                            td { {item.id} }
                            td { {markup::raw(v_htmlescape::escape(&item.message))} }
                        }
                    }
                }
            }
        }
    }
}

struct PgConnectionPool {
    idx: AtomicUsize,
    clients: Vec<Arc<PgConnection>>,
}

impl PgConnectionPool {
    fn new(db_url: &str, size: usize) -> PgConnectionPool {
        let mut clients = Vec::with_capacity(size);
        for _ in 0..size {
            let client = PgConnection::new(db_url);
            clients.push(Arc::new(client));
        }

        PgConnectionPool {
            idx: AtomicUsize::new(0),
            clients,
        }
    }

    fn get_connection(&self) -> (Arc<PgConnection>, usize) {
        let idx = self.idx.fetch_add(1, Ordering::Relaxed);
        let len = self.clients.len();
        (self.clients[idx % len].clone(), idx)
    }
}

struct PgConnection {
    client: Client,
    world: Statement,
    fortune: Statement,
}

impl PgConnection {
    fn new(db_url: &str) -> Self {
        let client = may_postgres::connect(db_url).unwrap();
        let world = client
            .prepare("SELECT id, randomnumber FROM world WHERE id=$1")
            .unwrap();

        let fortune = client.prepare("SELECT id, message FROM fortune").unwrap();

        PgConnection {
            client,
            world,
            fortune,
        }
    }

    fn get_world(&self, random_id: i32) -> Result<WorldRow, may_postgres::Error> {
        let mut q = self
            .client
            .query_raw(&self.world, utils::slice_iter(&[&random_id]))?;
        match q.next().transpose()? {
            Some(row) => Ok(WorldRow {
                id: row.get(0),
                randomnumber: row.get(1),
            }),
            None => unreachable!(),
        }
    }

    fn get_worlds(
        &self,
        num: usize,
        rand: &mut Rand32,
    ) -> Result<Vec<WorldRow>, may_postgres::Error> {
        let mut queries = SmallVec::<[RowStream; 32]>::new();
        for _ in 0..num {
            let random_id = rand.rand_range(1..10001) as i32;
            queries.push(
                self.client
                    .query_raw(&self.world, utils::slice_iter(&[&random_id]))?,
            );
        }

        let mut worlds = Vec::with_capacity(num);
        for mut q in queries {
            match q.next().transpose()? {
                Some(row) => worlds.push(WorldRow {
                    id: row.get(0),
                    randomnumber: row.get(1),
                }),
                None => unreachable!(),
            }
        }
        Ok(worlds)
    }

    fn updates(&self, num: usize, rand: &mut Rand32) -> Result<Vec<WorldRow>, may_postgres::Error> {
        let mut queries = SmallVec::<[RowStream; 32]>::new();
        for _ in 0..num {
            let random_id = rand.rand_range(1..10001) as i32;
            queries.push(
                self.client
                    .query_raw(&self.world, utils::slice_iter(&[&random_id]))?,
            );
        }

        let mut worlds = Vec::with_capacity(num);
        for mut q in queries {
            match q.next().transpose()? {
                Some(row) => worlds.push(WorldRow {
                    id: row.get(0),
                    randomnumber: row.get(1),
                }),
                None => unreachable!(),
            }
        }

        let mut update = String::with_capacity(120 + 12 * num);
        update.push_str("UPDATE world SET randomnumber = temp.randomnumber FROM (VALUES ");

        for w in &mut worlds {
            w.randomnumber = rand.rand_range(1..10001) as i32;
            let _ = write!(&mut update, "({}, {}),", w.id, w.randomnumber);
        }
        update.pop();
        update.push_str(" ORDER BY 1) AS temp(id, randomnumber) WHERE temp.id = world.id");

        self.client.simple_query(&update)?;
        Ok(worlds)
    }

    fn tell_fortune(&self) -> Result<Vec<Fortune>, may_postgres::Error> {
        let mut items = Vec::with_capacity(80);
        items.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });

        let rows = self
            .client
            .query_raw(&self.fortune, utils::slice_iter(&[]))?;

        for row in rows {
            let r = row?;
            items.push(Fortune {
                id: r.get(0),
                message: r.get(1),
            });
        }

        items.sort_by(|it, next| it.message.cmp(&next.message));
        Ok(items)
    }
}

struct Techempower {
    db: Arc<PgConnection>,
    rng: Rand32,
}

impl HttpService for Techempower {
    fn call(&mut self, req: Request, rsp: &mut Response) -> io::Result<()> {
        // Bare-bones router
        match req.path() {
            "/json" => {
                rsp.header("Content-Type: application/json");
                serde_json::to_writer(
                    BodyWriter(rsp.body_mut()),
                    &HeloMessage {
                        message: "Hello, World!",
                    },
                )?;
            }
            "/plaintext" => {
                rsp.header("Content-Type: text/plain").body("Hello, World!");
            }
            "/db" => {
                let random_id = self.rng.rand_range(1..10001) as i32;
                let world = self.db.get_world(random_id).unwrap();
                rsp.header("Content-Type: application/json");
                serde_json::to_writer(BodyWriter(rsp.body_mut()), &world)?;
            }
            "/fortune" => {
                let fortunes = self.db.tell_fortune().unwrap();
                rsp.header("Content-Type: text/html; charset=utf-8");
                write!(rsp.body_mut(), "{}", FortunesTemplate { fortunes }).unwrap();
            }
            p if p.starts_with("/queries") => {
                let q = utils::get_query_param(p) as usize;
                let worlds = self.db.get_worlds(q, &mut self.rng).unwrap();
                rsp.header("Content-Type: application/json");
                serde_json::to_writer(BodyWriter(rsp.body_mut()), &worlds)?;
            }
            p if p.starts_with("/updates") => {
                let q = utils::get_query_param(p) as usize;
                let worlds = self.db.updates(q, &mut self.rng).unwrap();
                rsp.header("Content-Type: application/json");
                serde_json::to_writer(BodyWriter(rsp.body_mut()), &worlds)?;
            }
            _ => {
                rsp.status_code("404", "Not Found");
            }
        }

        Ok(())
    }
}

struct HttpServer {
    db_pool: PgConnectionPool,
}

impl HttpServiceFactory for HttpServer {
    type Service = Techempower;

    fn new_service(&self) -> Self::Service {
        let (db, idx) = self.db_pool.get_connection();
        let rng = Rand32::new(idx as u64);
        Techempower { db, rng }
    }
}

fn main() {
    may::config()
        .set_pool_capacity(10000)
        .set_stack_size(0x1000);
    let server = HttpServer {
        db_pool: PgConnectionPool::new(
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world",
            num_cpus::get(),
        ),
    };
    server.start("0.0.0.0:8080").unwrap().join().unwrap();
}
