use std::borrow::Cow;

use nanorand::{Rng, WyRand};

pub use sqlx::{
    pool::PoolConnection,
    postgres::{PgArguments, PgPool, PgPoolOptions, PgRow},
    Arguments, Pool, Postgres, Row,
};

use viz::{
    async_trait, Error, FromRequest, IntoResponse, Request, RequestExt, Response,
    StatusCode,
};

use crate::models_sqlx::*;
use crate::utils::get_query_param;
use crate::RANGE;

pub struct DatabaseConnection(pub PoolConnection<Postgres>);

#[async_trait]
impl FromRequest for DatabaseConnection {
    type Error = PgError;

    async fn extract(req: &mut Request) -> Result<Self, Self::Error> {
        req.state::<PgPool>()
            .ok_or(PgError(sqlx::Error::Io(std::io::Error::from(
                std::io::ErrorKind::NotConnected,
            ))))?
            .acquire()
            .await
            .map(Self)
            .map_err(PgError)
    }
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct PgError(#[from] pub sqlx::Error);

impl From<PgError> for Error {
    fn from(e: PgError) -> Self {
        Error::Responder(e.into_response())
    }
}

impl IntoResponse for PgError {
    fn into_response(self) -> Response {
        (StatusCode::INTERNAL_SERVER_ERROR, self.to_string()).into_response()
    }
}

pub struct Counter(pub u16);

#[async_trait]
impl FromRequest for Counter {
    type Error = Error;

    async fn extract(req: &mut Request) -> Result<Self, Self::Error> {
        Ok(Counter(get_query_param(req.query_string())))
    }
}

pub async fn get_worlds_by_limit(
    mut conn: PoolConnection<Postgres>,
    limit: i64,
) -> Result<Vec<World>, PgError> {
    let worlds = sqlx::query_as("SELECT * FROM World LIMIT $1")
        .bind(limit)
        .fetch_all(&mut conn)
        .await?;
    Ok(worlds)
}

pub async fn get_world(
    conn: &mut PoolConnection<Postgres>,
    id: i32,
) -> Result<World, PgError> {
    let world =
        sqlx::query_as::<_, World>("SELECT id, randomnumber FROM World WHERE id = $1")
            .bind(id)
            .fetch_one(conn)
            .await?;
    Ok(world)
}

pub async fn update_worlds(
    mut conn: PoolConnection<Postgres>,
    mut rng: WyRand,
    count: u16,
) -> Result<Vec<World>, PgError> {
    let mut worlds = Vec::<World>::with_capacity(count as usize);

    for _ in 0..count {
        let id = rng.generate_range(RANGE);
        let rid = rng.generate_range(RANGE);
        let mut w = get_world(&mut conn, id).await?;
        w.randomnumber = rid;
        worlds.push(w);
    }

    for w in &worlds {
        let mut args = PgArguments::default();
        args.add(w.randomnumber);
        args.add(w.id);

        sqlx::query_with("UPDATE World SET randomNumber = $1 WHERE id = $2", args)
            .execute(&mut conn)
            .await?;
    }

    Ok(worlds)
}

pub async fn get_fortunes(
    mut conn: PoolConnection<Postgres>,
) -> Result<Vec<Fortune>, PgError> {
    let mut items = sqlx::query("SELECT * FROM Fortune")
        .map(|row: PgRow| Fortune {
            id: row.get(0),
            message: Cow::Owned(row.get(1)),
        })
        .fetch_all(&mut conn)
        .await?;

    items.push(Fortune {
        id: 0,
        message: Cow::Borrowed("Additional fortune added at request time."),
    });

    items.sort_by(|it, next| it.message.cmp(&next.message));

    Ok(items)
}
