use deadpool_postgres::{Client, Manager, ManagerConfig, RecyclingMethod};
use tokio_pg_mapper::FromTokioPostgresRow;
use tokio_postgres::{NoTls, Row, Error, Statement};

use crate::{Fortune, World};


pub async fn create_pool(database_url: String, max_pool_size: u32) -> deadpool_postgres::Pool {
    let pg_config: tokio_postgres::Config = database_url.parse().expect("invalid database url");

    let mgr_config = ManagerConfig {
        recycling_method: RecyclingMethod::Fast,
    };
    let mgr = Manager::from_config(pg_config, NoTls, mgr_config);
    let pool: deadpool_postgres::Pool = deadpool_postgres::Pool::builder(mgr)
        .max_size(max_pool_size as usize)
        .build()
        .unwrap();

    pool
}

pub async fn fetch_world_by_id(client: &Client, number: i32, select: &Statement) -> Result<World, Error> {
    let row: Row = client.query_one(select, &[&number]).await.unwrap();

    Ok(World::from_row(row).unwrap())
}

pub async fn update_world(client: &Client, update: &Statement, random_id: i32, w_id: i32) -> Result<u64, Error> {
    let rows_modified: u64 = client.execute(update, &[&random_id, &w_id]).await.unwrap();

    Ok(rows_modified)
}

pub async fn fetch_all_fortunes(client: Client, select: &Statement) -> Result<Vec<Fortune>, Error> {
    let rows: Vec<Row> = client.query(select, &[]).await.unwrap();

    let mut fortunes: Vec<Fortune> = Vec::with_capacity(rows.capacity());

    for row in rows {
        fortunes.push(Fortune::from_row(row).unwrap());
    }

    Ok(fortunes)
}

pub async fn prepare_fetch_all_fortunes_statement(client: &Client) -> Statement {
    client.prepare_cached("SELECT * FROM Fortune").await.unwrap()
}

pub async fn prepare_fetch_world_by_id_statement(client: &Client) -> Statement {
    client
        .prepare_cached("SELECT id, randomnumber FROM World WHERE id = $1")
        .await
        .unwrap()
}

pub async fn prepare_update_world_by_id_statement(client: &Client) -> Statement {
    client
        .prepare_cached("UPDATE World SET randomnumber = $1 WHERE id = $2")
        .await
        .unwrap()
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
