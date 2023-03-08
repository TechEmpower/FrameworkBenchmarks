#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fmt::Write;
use std::io;
use std::sync::Arc;

use bytes::BytesMut;
use may_minihttp::{HttpService, HttpServiceFactory, Request, Response};
use may_postgres::{self, types::ToSql, Client, Statement};
use nanorand::{Rng, WyRand};
use smallvec::SmallVec;
use yarte::{ywrite_html, Serialize};

mod utils {
    use atoi::FromRadix10;

    pub fn get_query_param(query: &str) -> u16 {
        let q = if let Some(pos) = query.find("?q") {
            u16::from_radix_10(query.split_at(pos + 3).1.as_ref()).0
        } else {
            1
        };
        q.clamp(1, 500)
    }
}

#[derive(Serialize)]
struct HelloMessage {
    message: &'static str,
}

#[derive(Serialize)]
struct WorldRow {
    id: i32,
    randomnumber: i32,
}

#[derive(Serialize)]
pub struct Fortune<'a> {
    id: i32,
    message: &'a str,
}

struct PgConnectionPool {
    clients: Vec<PgConnection>,
}

impl PgConnectionPool {
    fn new(db_url: &'static str, size: usize) -> PgConnectionPool {
        let clients = (0..size)
            .map(|_| std::thread::spawn(move || PgConnection::new(db_url)))
            .collect::<Vec<_>>();
        let mut clients: Vec<_> = clients.into_iter().map(|t| t.join().unwrap()).collect();
        clients.sort_by(|a, b| (a.client.id() % size).cmp(&(b.client.id() % size)));
        PgConnectionPool { clients }
    }

    fn get_connection(&self, id: usize) -> PgConnection {
        let len = self.clients.len();
        let connection = &self.clients[id % len];
        PgConnection {
            client: connection.client.clone(),
            statement: connection.statement.clone(),
        }
    }
}

struct PgStatement {
    world: Statement,
    fortune: Statement,
    updates: Vec<Statement>,
}

struct PgConnection {
    client: Client,
    statement: Arc<PgStatement>,
}

impl PgConnection {
    fn new(db_url: &str) -> Self {
        let client = may_postgres::connect(db_url).unwrap();
        let world = client
            .prepare("SELECT id, randomnumber FROM world WHERE id=$1")
            .unwrap();

        let fortune = client.prepare("SELECT id, message FROM fortune").unwrap();

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
                let _ = write!(&mut q, "${pl},");
                pl += 1;
            }
            q.pop();
            q.push(')');
            updates.push(client.prepare(&q).unwrap());
        }

        let statement = Arc::new(PgStatement {
            world,
            fortune,
            updates,
        });

        PgConnection { client, statement }
    }

    fn get_world(&self, random_id: i32) -> Result<WorldRow, may_postgres::Error> {
        let mut q = self
            .client
            .query_raw(&self.statement.world, [&random_id as _])?;
        match q.next().transpose()? {
            Some(row) => Ok(WorldRow {
                id: row.get(0),
                randomnumber: row.get(1),
            }),
            None => unreachable!("random_id={}", random_id),
        }
    }

    fn get_worlds(
        &self,
        num: usize,
        rand: &mut WyRand,
    ) -> Result<SmallVec<[WorldRow; 32]>, may_postgres::Error> {
        let mut queries = SmallVec::<[_; 32]>::new();
        for _ in 0..num {
            let random_id = (rand.generate::<u32>() % 10_000 + 1) as i32;
            queries.push(
                self.client
                    .query_raw(&self.statement.world, [&random_id as _])?,
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
        rand: &mut WyRand,
    ) -> Result<SmallVec<[WorldRow; 32]>, may_postgres::Error> {
        let mut queries = SmallVec::<[_; 32]>::new();
        for _ in 0..num {
            let random_id = (rand.generate::<u32>() % 10_000 + 1) as i32;
            queries.push(
                self.client
                    .query_raw(&self.statement.world, [&random_id as _])?,
            );
        }

        let mut worlds = SmallVec::<[_; 32]>::new();
        for mut q in queries {
            let new_random_num = (rand.generate::<u32>() % 10_000 + 1) as i32;
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

        // use `query_one` to sync wait result
        let _ = self
            .client
            .query_one(&self.statement.updates[num - 1], &params);
        Ok(worlds)
    }

    fn tell_fortune(&self, buf: &mut BytesMut) -> Result<(), may_postgres::Error> {
        let rows = self.client.query_raw(&self.statement.fortune, [])?;

        let all_rows = Vec::from_iter(rows.map(|r| r.unwrap()));
        let mut fortunes = Vec::with_capacity(all_rows.capacity() + 1);
        fortunes.extend(all_rows.iter().map(|r| Fortune {
            id: r.get(0),
            message: r.get(1),
        }));
        fortunes.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.",
        });
        fortunes.sort_by(|it, next| it.message.cmp(next.message));

        let mut body = std::mem::replace(buf, BytesMut::new());
        ywrite_html!(body, "{{> fortune }}");
        let _ = std::mem::replace(buf, body);
        Ok(())
    }
}

struct Techempower {
    db: PgConnection,
    rng: WyRand,
}

impl HttpService for Techempower {
    fn call(&mut self, req: Request, rsp: &mut Response) -> io::Result<()> {
        // Bare-bones router
        match req.path() {
            "/json" => {
                rsp.header("Content-Type: application/json");
                let msg = HelloMessage {
                    message: "Hello, World!",
                };
                msg.to_bytes_mut(rsp.body_mut());
            }
            "/plaintext" => {
                rsp.header("Content-Type: text/plain").body("Hello, World!");
            }
            "/db" => {
                rsp.header("Content-Type: application/json");
                let random_id = (self.rng.generate::<u32>() % 10_000 + 1) as i32;
                let world = self.db.get_world(random_id).unwrap();
                world.to_bytes_mut(rsp.body_mut())
            }
            "/fortunes" => {
                rsp.header("Content-Type: text/html; charset=utf-8");
                self.db.tell_fortune(rsp.body_mut()).unwrap();
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

    fn new_service(&self, id: usize) -> Self::Service {
        let db = self.db_pool.get_connection(id);
        let rng = WyRand::new();
        Techempower { db, rng }
    }
}

fn main() {
    may::config().set_pool_capacity(1000).set_stack_size(0x1000);
    println!("Starting http server: 127.0.0.1:8080");
    let server = HttpServer {
        db_pool: PgConnectionPool::new(
            "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world",
            num_cpus::get(),
        ),
    };
    server.start("0.0.0.0:8080").unwrap().join().unwrap();
}
