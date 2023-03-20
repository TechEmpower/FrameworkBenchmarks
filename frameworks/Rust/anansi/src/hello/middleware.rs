use std::fmt;
use std::sync::Arc;
use anansi::db::postgres::{PgDbRow, PgDbRowVec, PgStatement};

#[macro_export]
macro_rules! impl_pg {
    () => {
        #[async_trait::async_trait]
        impl crate::hello::middleware::Pg for HttpRequest {
            async fn get_world(&self) -> anansi::web::Result<anansi::db::postgres::PgDbRow> {
                use anansi::web::BaseRequest;
                self.mid.stmt.0.world.fetch_one(&[&Self::random_num()], self.raw().pool()).await
            }
            async fn get_fortunes(&self) -> anansi::web::Result<anansi::db::postgres::PgDbRowVec> {
                use anansi::web::BaseRequest;
                self.mid.stmt.0.fortune.fetch_all(&[], self.raw().pool()).await
            }
        }
    }
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
    pub async fn new(raw: &mut anansi::web::RawRequest<anansi::db::postgres::PgDbPool>) -> anansi::web::Result<Self> {
        Ok(Self(Arc::new(State {
            world: PgStatement::new("SELECT * FROM world WHERE id = $1", raw.pool()).await?,
            fortune: PgStatement::new("SELECT * FROM fortune", raw.pool()).await?,
        })))
    }
}

pub struct State {
    pub world: PgStatement,
    pub fortune: PgStatement,
}

#[async_trait::async_trait]
pub trait Pg {
    fn random_num() -> i32 {
        use rand::Rng;
        rand::thread_rng().gen_range(1..=10_000)
    }
    async fn get_world(&self) -> anansi::web::Result<PgDbRow>;
    async fn get_fortunes(&self) -> anansi::web::Result<PgDbRowVec>;
}
