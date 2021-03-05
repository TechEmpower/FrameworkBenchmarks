#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

use anyhow::Error;
use futures::{pin_mut, TryStreamExt};
use hyper::server::conn::AddrIncoming;
use once_cell::sync::OnceCell;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write;
use std::{cmp, io};
use tokio_postgres::types::ToSql;
use tokio_postgres::{self, Client, NoTls, Statement};
use yarte::ywrite_html;

mod models;
use models::*;

const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
pub static DB_CONN: OnceCell<PgConnection> = OnceCell::new();
pub fn connect() -> &'static PgConnection {
    DB_CONN.get().unwrap()
}
pub struct PgConnection {
    client: Client,
    fortune: Statement,
    world: Statement,
    updates: HashMap<u16, Statement>,
}

#[derive(Serialize, Debug)]
pub struct CowFortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

impl PgConnection {
    pub async fn connect(db_url: &str) -> Result<PgConnection, io::Error> {
        let (client, conn) = tokio_postgres::connect(db_url, NoTls)
            .await
            .expect("can not connect to postgresql");
        tokio::spawn(async move {
            if let Err(e) = conn.await {
                eprintln!("connection error: {}", e);
            }
        });

        let fortune = client.prepare("SELECT * FROM fortune").await.unwrap();
        let world = client.prepare("SELECT * FROM world WHERE id=$1").await.unwrap();
        let mut updates_statements = HashMap::new();
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
            updates_statements.insert(num, client.prepare(&q).await.unwrap());
        }

        Ok(PgConnection {
            client,
            fortune,
            world,
            updates: updates_statements,
        })
    }
}

#[fn_handler]
async fn world_row(_req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let conn = connect();
    let mut rng = SmallRng::from_entropy();
    let random_id = (rng.gen::<u32>() % 10_000 + 1) as i32;
    let row = conn.client.query_one(&conn.world, &[&random_id]).await?;
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    let world = &World {
        id: row.get(0),
        randomnumber: row.get(1),
    };
    res.render_json(&world);
    Ok(())
}

#[fn_handler]
async fn queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.get_query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));

    let mut worlds = Vec::with_capacity(count);
    let mut rng = SmallRng::from_entropy();
    let conn = connect();
    for _ in 0..count {
        let w_id = (rng.gen::<u32>() % 10_000 + 1) as i32;
        let row = conn.client.query_one(&conn.world, &[&w_id]).await?;
        worlds.push(World {
            id: row.get(0),
            randomnumber: row.get(1),
        });
    }
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_json(&worlds);
    Ok(())
}

#[fn_handler]
async fn updates(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.get_query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));

    let mut worlds = Vec::with_capacity(count);
    let mut rng = SmallRng::from_entropy();
    let conn = connect();
    for _ in 0..count {
        let id = (rng.gen::<u32>() % 10_000 + 1) as i32;
        let w_id = (rng.gen::<u32>() % 10_000 + 1) as i32;
        let row = conn.client.query_one(&conn.world, &[&w_id]).await?;
        worlds.push(World {
            id: row.get(0),
            randomnumber: id,
        });
    }

    let st = conn.updates.get(&(count as u16)).unwrap().clone();

    let mut params: Vec<&(dyn ToSql + std::marker::Sync)> = Vec::with_capacity(count * 3);
    for w in &worlds {
        params.push(&w.id);
        params.push(&w.randomnumber);
    }
    for w in &worlds {
        params.push(&w.id);
    }

    conn.client.query(&st, &params).await?;
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_json(&worlds);
    Ok(())
}

#[fn_handler]
async fn fortunes(_req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let mut items = vec![CowFortune {
        id: 0,
        message: Cow::Borrowed("Additional fortune added at request time."),
    }];
    let conn = connect();
    let params: [&str; 0] = [];
    let stream = conn.client.query_raw(&conn.fortune, &params).await?;
    pin_mut!(stream);

    while let Some(row) = stream.try_next().await? {
        items.push(CowFortune {
            id: row.get(0),
            message: Cow::Owned(row.get(1)),
        });
    }

    items.sort_by(|it, next| it.message.cmp(&next.message));

    let mut body = Vec::with_capacity(2048);
    ywrite_html!(body, "{{> fortune }}");

    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_binary("text/html; charset=utf-8".parse().unwrap(), &body);
    Ok(())
}

#[tokio::main]
async fn main() {
    println!("Started http server: 127.0.0.1:8080");

    DB_CONN
        .set(
            PgConnection::connect(DB_URL)
                .await
                .expect(&format!("Error connecting to {}", &DB_URL)),
        )
        .ok();
    let router = Router::new()
        .push(Router::new().path("db").get(world_row))
        .push(Router::new().path("fortunes").get(fortunes))
        .push(Router::new().path("queries").get(queries))
        .push(Router::new().path("updates").get(updates));
    Server::new(router).bind(([0, 0, 0, 0], 8080)).await;
}
