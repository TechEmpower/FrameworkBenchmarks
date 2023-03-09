use std::{cell::RefCell, error::Error, fmt, future::Future, io, time::Duration};

use diesel::prelude::{ConnectionError, ExpressionMethods, QueryDsl};
use diesel_async::{AsyncConnection, AsyncPgConnection, RunQueryDsl};
use futures_util::stream::{FuturesUnordered, TryStreamExt};
use tang_rs::{Manager, ManagerFuture, ManagerTimeout, Pool};
use tokio::time::{sleep, Sleep};

use super::{
    ser::{Fortune, Fortunes, World},
    util::Rand,
};

type DbResult<T> = Result<T, Box<dyn Error + Send + Sync + 'static>>;

pub struct DieselPoolManager(String);

impl Manager for DieselPoolManager {
    type Connection = AsyncPgConnection;
    type Error = DieselPoolError;
    type Timeout = Sleep;
    type TimeoutError = ();

    fn connect(&self) -> ManagerFuture<Result<Self::Connection, Self::Error>> {
        let url = self.0.clone();
        Box::pin(async move {
            let conn = AsyncPgConnection::establish(url.as_str()).await?;
            Ok(conn)
        })
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

pub struct DieselPool {
    rng: RefCell<Rand>,
    pool: Pool<DieselPoolManager>,
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
        rng: RefCell::new(Rand::default()),
        pool,
    })
}

impl DieselPool {
    pub async fn get_world(&self) -> DbResult<World> {
        use crate::schema::world::dsl::*;

        let mut conn = self.pool.get().await?;

        let random_id = self.rng.borrow_mut().gen_id();

        let w = world
            .filter(id.eq(random_id))
            .load::<World>(&mut *conn)
            .await?
            .pop()
            .unwrap();

        Ok(w)
    }

    pub async fn get_worlds(&self, num: u16) -> DbResult<Vec<World>> {
        use crate::schema::world::dsl::*;

        let worlds = {
            let mut conn = self.pool.get().await?;

            let mut rng = self.rng.borrow_mut();
            (0..num)
                .map(|_| {
                    let w_id = rng.gen_id();
                    let fut = world.filter(id.eq(w_id)).load::<World>(&mut *conn);
                    async {
                        let w = fut.await?.pop().unwrap();
                        Ok(w)
                    }
                })
                .collect::<FuturesUnordered<_>>()
        };

        worlds.try_collect().await
    }

    pub async fn update(&self, num: u16) -> DbResult<Vec<World>> {
        use crate::schema::world::dsl::*;

        let mut conn = self.pool.get().await?;

        let mut worlds = {
            let mut rng = self.rng.borrow_mut();
            (0..num)
                .map(|_| {
                    let w_id = rng.gen_id();
                    let new_id = rng.gen_id();
                    let fut = world.filter(id.eq(w_id)).load::<World>(&mut *conn);
                    async move {
                        let mut w = fut.await?.pop().unwrap();
                        w.randomnumber = new_id;
                        DbResult::Ok(w)
                    }
                })
                .collect::<FuturesUnordered<_>>()
        }
        .try_collect::<Vec<_>>()
        .await?;

        worlds.sort_by_key(|w| w.id);

        conn.transaction(move |conn| {
            Box::pin(async move {
                for w in &worlds {
                    diesel::update(world)
                        .filter(id.eq(w.id))
                        .set(randomnumber.eq(w.randomnumber))
                        .execute(conn)
                        .await?;
                }
                Ok(worlds)
            })
        })
        .await
    }

    pub async fn tell_fortune(&self) -> DbResult<Fortunes> {
        use crate::schema::fortune::dsl::*;

        let mut conn = self.pool.get().await?;

        let mut items = fortune.load::<Fortune>(&mut *conn).await?;

        items.push(Fortune::new(0, "Additional fortune added at request time."));
        items.sort_by(|it, next| it.message.cmp(&next.message));

        Ok(Fortunes::new(items))
    }
}
