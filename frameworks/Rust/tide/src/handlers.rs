//! Functions that handle each request, returning a valid response.

use crate::{
    db::Pool,
    models::{Fortune, Message, World},
    rand::random_10k,
};
use askama::Template;
use diesel::prelude::*;
use tide::{Body, Error, Request, Response, Result};

/// Return a static plaintext body.
pub async fn plaintext(_request: Request<Pool>) -> Result<Response> {
    Ok(Response::builder(200).body("Hello, World!").build())
}

/// Return a static JSON body.
pub async fn json(_request: Request<Pool>) -> Result<Response> {
    Ok(Response::builder(200)
        .body(Body::from_json(&Message {
            message: "Hello, World!",
        })?)
        .build())
}

/// Load a single random `World` from the database.
fn load_world(connection: &crate::db::Connection) -> Result<World> {
    use crate::schema::world::dsl::{id, world};

    Ok(world
        .filter(id.eq(random_10k()))
        .load::<World>(connection)
        .map_err(|error| Error::new(500, error))?
        .pop()
        .expect("one item in world query result"))
}

/// Return a single random `World` as JSON.
pub async fn db(request: Request<Pool>) -> Result<Response> {
    let connection = request.state().get()?;
    let loaded_world = load_world(&connection)?;

    Ok(Response::builder(200)
        .body(Body::from_json(&loaded_world)?)
        .build())
}

/// Get the count parameter from the request route, normalizing invalid values.
///
/// Return an integer in the range 1 - 500.
fn get_count_param<T>(request: &Request<T>) -> usize {
    const _COUNT_MIN: usize = 1;
    const _COUNT_MAX: usize = 500;

    let mut count = request
        .param("count")
        .unwrap_or("1")
        .parse()
        .unwrap_or(_COUNT_MIN);
    if count > _COUNT_MAX {
        count = _COUNT_MAX;
    } else if count < _COUNT_MIN {
        count = _COUNT_MIN;
    }
    count
}

/// Return a variable number of random `World`s as a JSON list.
pub async fn queries(request: Request<Pool>) -> Result<Response> {
    let count = get_count_param(&request);

    let mut loaded_worlds = Vec::with_capacity(count);
    let connection = request.state().get()?;
    for _index in 0..count {
        loaded_worlds.push(load_world(&connection)?);
    }

    Ok(Response::builder(200)
        .body(Body::from_json(&loaded_worlds)?)
        .build())
}

/// Template to render `Fortune`s as a HTML list.
#[derive(Template)]
#[template(path = "fortune.html")]
struct FortuneTemplate<'a> {
    items: &'a Vec<Fortune>,
}

/// Return all `Fortune`s rendered as HTML.
pub async fn fortunes(request: Request<Pool>) -> Result<Response> {
    use crate::schema::fortune::dsl::fortune;

    let connection = request.state().get()?;
    let mut items = fortune
        .load::<Fortune>(&connection)
        .map_err(|error| Error::new(500, error))?;
    items.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });
    items.sort_by(|item, next| item.message.cmp(&next.message));

    let template = FortuneTemplate { items: &items };
    let html = template.render().expect("template to render");

    Ok(Response::builder(200)
        .body(Body::from_string(html))
        .content_type(http_types::mime::HTML)
        .build())
}

/// Update a variable number of random `World`s, returning them as a JSON list.
pub async fn updates(request: Request<Pool>) -> Result<Response> {
    let count = get_count_param(&request);
    let connection = request.state().get()?;
    let mut loaded_worlds = Vec::with_capacity(count);
    for _index in 0..count {
        let mut loaded_world = load_world(&connection)?;
        loaded_world.randomnumber = random_10k();
        loaded_worlds.push(loaded_world);
    }

    // This line is required to pass test verification otherwise we get:
    //
    // ```
    // FAIL for http://tfb-server:8080/updates/20
    //   Only 20470 executed queries in the database out of roughly 20480 expected.
    // PASS for http://tfb-server:8080/updates/20
    //   Rows read: 10128/10240
    // FAIL for http://tfb-server:8080/updates/20
    //   Only 10118 rows updated in the database out of roughly 10240 expected.
    // ```
    //
    // I don't know why this is the case, but I copied it from the actix
    // implementation and here it shall stay.
    loaded_worlds.sort_by_key(|loaded_world| loaded_world.id);

    let _ = connection.transaction::<(), Error, _>(|| {
        for loaded_world in &loaded_worlds {
            use crate::schema::world::dsl::{id, randomnumber, world};
            diesel::update(world)
                .filter(id.eq(loaded_world.id))
                .set(randomnumber.eq(loaded_world.randomnumber))
                .execute(&connection)
                .map_err(|error| Error::new(500, error))?;
        }
        Ok(())
    });

    Ok(Response::builder(200)
        .body(Body::from_json(&loaded_worlds)?)
        .build())
}
