use sea_orm::{ConnectOptions, Database, DatabaseConnection};
use std::env;
use trillium::{async_trait, Conn, Handler, Info};

#[derive(Debug, Default)]
pub struct Db(Option<DatabaseConnection>);

pub mod fortune;
pub mod world;

#[async_trait]
impl Handler for Db {
    async fn run(&self, conn: Conn) -> Conn {
        conn.with_state(self.0.as_ref().unwrap().clone())
    }

    async fn init(&mut self, _info: &mut Info) {
        if self.0.is_none() {
            let db_url = env::var("DATABASE_URL").expect("env var DATABASE_URL not found");

            let connect_options = ConnectOptions::new(db_url.clone());

            let db = Database::connect(connect_options)
                .await
                .map_err(|e| format!("could not connect to {}: {}", &db_url, e))
                .unwrap();

            self.0 = Some(db);
        }
    }
}

pub trait DbConnExt {
    fn db(&self) -> &DatabaseConnection;
}

impl DbConnExt for Conn {
    fn db(&self) -> &DatabaseConnection {
        self.state().unwrap()
    }
}
