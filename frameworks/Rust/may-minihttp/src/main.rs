#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::borrow::Cow;
use std::fmt::Write;
use std::io;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use may_minihttp::{HttpService, HttpServiceFactory, Request, Response};
use may_postgres::{self, types::ToSql, Client, Statement};
use oorandom::Rand32;
use smallvec::SmallVec;
use yarte::{ywrite_html, Serialize};

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
    message: Cow<'static, str>,
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
    updates: Vec<Statement>,
}

impl PgConnection {
    fn new(db_url: &str) -> Self {
        let client = may_postgres::connect(db_url).unwrap();
        let world = client.prepare("SELECT * FROM world WHERE id=$1").unwrap();

        let fortune = client.prepare("SELECT * FROM fortune").unwrap();

        let mut updates = Vec::new();
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
            updates.push(client.prepare(&q).unwrap());
        }

        PgConnection {
            client,
            world,
            fortune,
            updates,
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
    ) -> Result<SmallVec<[WorldRow; 32]>, may_postgres::Error> {
        let mut queries = SmallVec::<[_; 32]>::new();
        for _ in 0..num {
            let random_id = (rand.rand_u32() % 10_000 + 1) as i32;
            queries.push(
                self.client
                    .query_raw(&self.world, utils::slice_iter(&[&random_id]))?,
            );
        }

        let mut worlds = SmallVec::<[_; 32]>::new();
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

    fn updates(
        &self,
        num: usize,
        rand: &mut Rand32,
    ) -> Result<SmallVec<[WorldRow; 32]>, may_postgres::Error> {
        let mut queries = SmallVec::<[_; 32]>::new();
        for _ in 0..num {
            let random_id = (rand.rand_u32() % 10_000 + 1) as i32;
            queries.push(
                self.client
                    .query_raw(&self.world, utils::slice_iter(&[&random_id]))?,
            );
        }

        let mut worlds = SmallVec::<[_; 32]>::new();
        for mut q in queries {
            let new_random_num = (rand.rand_u32() % 10_000 + 1) as i32;
            match q.next().transpose()? {
                Some(row) => worlds.push(WorldRow {
                    id: row.get(0),
                    randomnumber: new_random_num,
                }),
                None => unreachable!(),
            }
        }

        let mut params: Vec<&(dyn ToSql + Sync)> = Vec::with_capacity(num * 3);
        for w in &worlds {
            params.push(&w.id);
            params.push(&w.randomnumber);
        }
        for w in &worlds {
            params.push(&w.id);
        }

        self.client.query(&self.updates[num - 1], &params)?;
        Ok(worlds)
    }

    fn tell_fortune(&self) -> Result<Vec<Fortune>, may_postgres::Error> {
        let mut items = vec![Fortune {
            id: 0,
            message: Cow::Borrowed("Additional fortune added at request time."),
        }];

        let rows = self
            .client
            .query_raw(&self.fortune, utils::slice_iter(&[]))?;

        for row in rows {
            let r = row?;
            items.push(Fortune {
                id: r.get(0),
                message: Cow::Owned(r.get(1)),
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
                let msg = HeloMessage {
                    message: "Hello, World!",
                };
                msg.to_bytes_mut(rsp.body_mut());
            }
            "/plaintext" => {
                rsp.header("Content-Type: text/plain").body("Hello, World!");
            }
            "/db" => {
                rsp.header("Content-Type: application/json");
                let random_id = (self.rng.rand_u32() % 10_000 + 1) as i32;
                let world = self.db.get_world(random_id).unwrap();
                world.to_bytes_mut(rsp.body_mut())
            }
            "/fortunes" => {
                rsp.header("Content-Type: text/html; charset=utf-8");
                let fortunes = self.db.tell_fortune().unwrap();
                let mut body = Vec::with_capacity(2048);
                ywrite_html!(body, "{{> fortune }}");
                rsp.body_vec(body);
            }
            p if p.starts_with("/queries") => {
                rsp.header("Content-Type: application/json");
                let q = utils::get_query_param(p) as usize;
                let worlds = self.db.get_worlds(q, &mut self.rng).unwrap();
                worlds.to_bytes_mut(rsp.body_mut());
            }
            p if p.starts_with("/updates") => {
                rsp.header("Content-Type: application/json");
                let q = utils::get_query_param(p) as usize;
                let worlds = self.db.updates(q, &mut self.rng).unwrap();
                worlds.to_bytes_mut(rsp.body_mut());
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
    println!("Starting http server: 127.0.0.1:8080");
    let server = HttpServer {
        db_pool: PgConnectionPool::new(
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world",
            num_cpus::get(),
        ),
    };
    server.start("0.0.0.0:8080").unwrap().join().unwrap();
}
