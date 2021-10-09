use std::{error::Error, fmt, future::Future, io, time::Duration};

use diesel::prelude::*;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use tang_rs::{Manager, ManagerFuture, ManagerTimeout, Pool};
use tokio::{
    task::spawn_blocking,
    time::{sleep, Sleep},
};

use super::ser::{Fortune, Fortunes, World};

type DbResult<T> = Result<T, Box<dyn Error + Send + Sync + 'static>>;

pub struct DieselPoolManager(String);

impl Manager for DieselPoolManager {
    type Connection = PgConnection;
    type Error = DieselPoolError;
    type Timeout = Sleep;
    type TimeoutError = ();

    fn connect(&self) -> ManagerFuture<Result<Self::Connection, Self::Error>> {
        let conn = PgConnection::establish(self.0.as_str());
        Box::pin(async move { Ok(conn?) })
    }

    fn is_valid<'a>(
        &'a self,
        _: &'a mut Self::Connection,
    ) -> ManagerFuture<'a, Result<(), Self::Error>> {
        Box::pin(async { Ok(()) })
    }

    fn is_closed(&self, _: &mut Self::Connection) -> bool {
        false
    }

    fn spawn<Fut>(&self, fut: Fut)
    where
        Fut: Future<Output = ()> + 'static,
    {
        tokio::task::spawn_local(fut);
    }

    fn timeout<Fut: Future>(&self, fut: Fut, dur: Duration) -> ManagerTimeout<Fut, Self::Timeout> {
        ManagerTimeout::new(fut, sleep(dur))
    }
}

pub enum DieselPoolError {
    Inner(ConnectionError),
    TimeOut,
}

impl fmt::Debug for DieselPoolError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DieselPoolError::Inner(e) => e.fmt(f),
            DieselPoolError::TimeOut => f
                .debug_struct("DieselPoolError")
                .field("source", &"Connection Timeout")
                .finish(),
        }
    }
}

impl fmt::Display for DieselPoolError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for DieselPoolError {}

impl From<ConnectionError> for DieselPoolError {
    fn from(e: ConnectionError) -> Self {
        Self::Inner(e)
    }
}

impl From<()> for DieselPoolError {
    fn from(_: ()) -> Self {
        Self::TimeOut
    }
}

#[derive(Clone)]
pub struct DieselPool {
    pool: Pool<DieselPoolManager>,
    rng: SmallRng,
}

pub async fn create(config: &str) -> io::Result<DieselPool> {
    let pool = tang_rs::Builder::new()
        .max_size(5)
        .min_idle(5)
        .always_check(false)
        .idle_timeout(None)
        .max_lifetime(None)
        .build(DieselPoolManager(String::from(config)))
        .await
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    Ok(DieselPool {
        pool,
        rng: SmallRng::from_entropy(),
    })
}

impl DieselPool {
    pub async fn get_world(&self) -> DbResult<World> {
        let mut rng = self.rng.clone();
        let conn = self.pool.get_owned().await?;

        spawn_blocking(move || {
            use crate::schema::world::dsl::*;

            let random_id = rng.gen_range(1..10_001);
            let w = world
                .filter(id.eq(random_id))
                .load::<World>(&*conn)?
                .pop()
                .unwrap();

            Ok(w)
        })
        .await?
    }

    pub async fn get_worlds(&self, num: u16) -> DbResult<Vec<World>> {
        let mut rng = self.rng.clone();
        let conn = self.pool.get_owned().await?;

        spawn_blocking(move || {
            use crate::schema::world::dsl::*;

            (0..num)
                .map(|_| {
                    let w_id = rng.gen_range(1..10_001);
                    let w = world
                        .filter(id.eq(w_id))
                        .load::<World>(&*conn)?
                        .pop()
                        .unwrap();
                    Ok(w)
                })
                .collect()
        })
        .await?
    }

    pub async fn update(&self, num: u16) -> DbResult<Vec<World>> {
        let mut rng = self.rng.clone();
        let conn = self.pool.get_owned().await?;

        spawn_blocking(move || {
            use crate::schema::world::dsl::*;

            let mut worlds = (0..num)
                .map(|_| {
                    let w_id: i32 = rng.gen_range(1..10_001);
                    let mut w = world
                        .filter(id.eq(w_id))
                        .load::<World>(&*conn)?
                        .pop()
                        .unwrap();
                    w.randomnumber = rng.gen_range(1..10_001);
                    Ok(w)
                })
                .collect::<DbResult<Vec<_>>>()?;

            worlds.sort_by_key(|w| w.id);

            conn.transaction::<_, diesel::result::Error, _>(|| {
                for w in &worlds {
                    diesel::update(world)
                        .filter(id.eq(w.id))
                        .set(randomnumber.eq(w.randomnumber))
                        .execute(&*conn)?;
                }
                Ok(())
            })?;

            Ok(worlds)
        })
        .await?
    }

    pub async fn tell_fortune(&self) -> DbResult<Fortunes> {
        let conn = self.pool.get_owned().await?;

        spawn_blocking(move || {
            use crate::schema::fortune::dsl::*;

            let mut items = fortune.load::<Fortune>(&*conn)?;

            items.push(Fortune::new(0, "Additional fortune added at request time."));
            items.sort_by(|it, next| it.message.cmp(&next.message));

            Ok(Fortunes::new(items))
        })
        .await?
    }
}
