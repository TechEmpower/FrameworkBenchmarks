use std::borrow::Cow;

use diesel::prelude::*;
use diesel_async::{
    pooled_connection::bb8::{Pool, RunError},
    AsyncPgConnection, RunQueryDsl,
};
use nanorand::{Rng, WyRand};
use sailfish::{RenderError, TemplateOnce};
use viz::{Error, IntoResponse, Response, StatusCode};

use crate::models_diesel::*;
use crate::schema::*;
use crate::RANGE;

/// Postgres Error
#[derive(Debug, thiserror::Error)]
pub enum PgError {
    #[error("missing pool")]
    Missing,
    #[error(transparent)]
    DieselError(#[from] diesel::result::Error),
    #[error(transparent)]
    PoolError(#[from] RunError),
    #[error(transparent)]
    RenderError(#[from] RenderError),
}

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

pub async fn get_worlds_by_limit(
    pool: Pool<AsyncPgConnection>,
    limit: i64,
) -> Result<Vec<World>, PgError> {
    let mut conn = pool.get().await?;
    let worlds = world::table
        .limit(limit)
        .get_results::<World>(&mut conn)
        .await?;
    Ok(worlds)
}

async fn _get_world(conn: &mut AsyncPgConnection, id: i32) -> Result<World, PgError> {
    let world = world::table.find(id).first(conn).await?;
    Ok(world)
}

pub async fn get_world(
    pool: Pool<AsyncPgConnection>,
    id: i32,
) -> Result<World, PgError> {
    let mut conn = pool.get().await?;
    _get_world(&mut conn, id).await
}

pub async fn get_worlds(
    pool: Pool<AsyncPgConnection>,
    mut rng: WyRand,
    count: u16,
) -> Result<Vec<World>, PgError> {
    let mut conn = pool.get().await?;

    let mut worlds = Vec::<World>::with_capacity(count as usize);

    for _ in 0..count {
        let id = rng.generate_range(RANGE);
        let w = _get_world(&mut conn, id).await?;
        worlds.push(w);
    }

    Ok(worlds)
}

pub async fn update_worlds(
    pool: Pool<AsyncPgConnection>,
    mut rng: WyRand,
    count: u16,
) -> Result<Vec<World>, PgError> {
    let mut conn = pool.get().await?;

    let mut worlds = Vec::<World>::with_capacity(count as usize);

    for _ in 0..count {
        let id = rng.generate_range(RANGE);
        let rid = rng.generate_range(RANGE);
        let mut w = _get_world(&mut conn, id).await?;
        w.randomnumber = rid;
        worlds.push(w);
    }

    worlds.sort_by_key(|w| w.id);

    conn.build_transaction()
        .run(move |conn| {
            Box::pin(async move {
                for w in &worlds {
                    diesel::update(world::table)
                        .filter(world::id.eq(w.id))
                        .set(world::randomnumber.eq(w.randomnumber))
                        .execute(conn)
                        .await?;
                }

                Ok::<_, PgError>(worlds)
            })
        })
        .await
}

pub async fn tell_fortune(pool: Pool<AsyncPgConnection>) -> Result<String, PgError> {
    let mut conn = pool.get().await?;

    let mut items = fortune::table.load::<Fortune>(&mut conn).await?;

    items.push(Fortune {
        id: 0,
        message: Cow::Borrowed("Additional fortune added at request time."),
    });
    items.sort_by(|it, next| it.message.cmp(&next.message));

    let html = Fortunes::new(items).render_once()?;

    Ok(html)
}
