use std::fmt::{self, Write};
use std::sync::Arc;
use anansi::db::postgres::{PgDbPool, PgDbRow, PgDbRowVec, PgStatement};

#[macro_export]
macro_rules! impl_pg {
    () => {
        #[async_trait::async_trait]
        impl crate::hello::middleware::Pg for HttpRequest {
            async fn get_world(&self) -> anansi::web::Result<anansi::db::postgres::PgDbRow> {
                use anansi::web::BaseRequest;
                self.raw().app_state().stmt.0.world.fetch_one(&[&Self::random_num()], self.raw().pool()).await
            }
            async fn update_worlds(&self, n: usize, params: &[&(dyn tokio_postgres::types::ToSql + Sync)]) -> anansi::web::Result<()> {
                use anansi::web::BaseRequest;
                self.raw().app_state().stmt.0.updates[n].execute(params, self.raw().pool()).await
            }
            async fn get_fortunes(&self) -> anansi::web::Result<anansi::db::postgres::PgDbRowVec> {
                use anansi::web::BaseRequest;
                self.raw().app_state().stmt.0.fortune.fetch_all(&[], self.raw().pool()).await
            }
        }
        impl crate::hello::middleware::AsStmt for AppData {
            fn as_stmt(&self) -> &crate::hello::middleware::Stmt {
                &self.stmt
            }
        }
    }
}

fn update_statement(num: u16) -> String {
    let mut pl = 1;
    let mut q = "UPDATE world SET randomnumber = CASE id ".to_string();
    for _ in 1..=num {
        let _ = write!(q, "WHEN ${} THEN ${} ", pl, pl + 1);
        pl += 2;
    }

    q.push_str("ELSE randomnumber END WHERE id IN (");

    for _ in 1..=num {
        let _ = write!(q, "${},", pl);
        pl += 1;
    }

    q.pop();
    q.push(')');
    q
}

#[derive(Clone)]
pub struct Stmt(pub Arc<State>);

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Stmt")
         .finish()
    }
}

impl Stmt {
    pub async fn new(pool: &PgDbPool) -> anansi::web::Result<Self> {
        let mut updates = vec![];
        for n in 1..=500 {
            updates.push(PgStatement::new(&update_statement(n), pool).await?);
        }
        Ok(Self(Arc::new(State {
            world: PgStatement::new("SELECT * FROM world WHERE id = $1", pool).await?,
            updates,
            fortune: PgStatement::new("SELECT * FROM fortune", pool).await?,
        })))
    }
}

pub struct State {
    pub world: PgStatement,
    pub updates: Vec<PgStatement>,
    pub fortune: PgStatement,
}

pub trait AsStmt {
    fn as_stmt(&self) -> &Stmt;
}

#[async_trait::async_trait]
pub trait Pg {
    fn random_num() -> i32 {
        use rand::Rng;
        rand::thread_rng().gen_range(1..=10_000)
    }
    async fn get_world(&self) -> anansi::web::Result<PgDbRow>;
    async fn update_worlds(&self, n: usize, params: &[&(dyn tokio_postgres::types::ToSql + Sync)]) -> anansi::web::Result<()>;
    async fn get_fortunes(&self) -> anansi::web::Result<PgDbRowVec>;
}
