use ohkami::prelude::*;
use crate::Postgres;


#[derive(Clone)]
pub struct SetServer;
impl FangAction for SetServer {
    #[inline(always)]
    async fn back<'a>(&'a self, res: &'a mut ohkami::Response) {
        res.headers.set().Server("ohkami");
    }
}

impl Postgres {
    pub async fn init() -> impl FangAction {
        #[derive(Clone)]
        pub struct UsePostgres(Postgres);
        impl FangAction for UsePostgres {
            #[inline(always)]
            async fn fore<'a>(&'a self, req: &'a mut Request) -> Result<(), Response> {
                Ok(req.memorize(self.0.clone()))
            }
        }

        macro_rules! load_env {
            ($($name:ident as $t:ty)*) => {$(
                #[allow(non_snake_case)]
                let $name = ::std::env::var(stringify!($name))
                    .expect(concat!(
                        "Failed to load environment variable ",
                        "`", stringify!($name), "`"
                    ))
                    .parse::<$t>()
                    .unwrap();
            )*};
        } load_env! {
            MAX_CONNECTIONS as u32
            MIN_CONNECTIONS as u32
            DATABASE_URL    as String
        }
            
        let pool = sqlx::postgres::PgPoolOptions::new()
            .max_connections(MAX_CONNECTIONS)
            .min_connections(MIN_CONNECTIONS)
            .connect(&DATABASE_URL).await
            .unwrap();
            
        UsePostgres(pool.into())
    }
}
