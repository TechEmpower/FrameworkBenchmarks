use std::convert::Infallible;
use axum::body::{Bytes, Full};
use axum::http::{header, HeaderValue, Response, StatusCode};
use axum::Json;
use axum::response::IntoResponse;
use rand::Rng;
use rand::rngs::SmallRng;
use serde::{Deserialize};
use yarte::Template;

use crate::models::{Fortune, Message};

#[derive(Debug, Deserialize)]
pub struct Params {
    queries: Option<String>,
}

pub async fn plaintext() -> &'static str {
    "Hello, World!"
}

pub async fn json() -> impl IntoResponse {
    let message = Message {
        message: "Hello, World!",
    };

    (StatusCode::OK, Json(message))
}

#[derive(Template)]
#[template(path = "fortunes.html.hbs")]
pub struct FortunesTemplate<'a> {
    pub fortunes: &'a Vec<Fortune>,
}

pub fn random_number(rng: &mut SmallRng) -> i32 {
    (rng.gen::<u32>() % 10_000 + 1) as i32
}

pub fn parse_params(params: Params) -> i32 {
    let mut q = 0;

    if params.queries.is_some() {
        let queries = params.queries.ok_or("could not get value").unwrap();

        let queries_as_int = queries.parse::<i32>();

        match queries_as_int {
            Ok(_ok) => q = queries_as_int.unwrap(),
            Err(_e) => q = 1,
        }
    }

    let q = if q == 0 {
        1
    } else if q > 500 {
        500
    } else {
        q
    };

    q
}

/// Utility function for mapping any error into a `500 Internal Server Error`
/// response.
pub fn internal_error<E>(err: E) -> (StatusCode, String)
    where
        E: std::error::Error,
{
    (StatusCode::INTERNAL_SERVER_ERROR, err.to_string())
}

#[derive(Clone, Copy, Debug)]
pub struct Utf8Html<T>(pub T);

impl<T> IntoResponse for Utf8Html<T>
    where
        T: Into<Full<Bytes>>,
{
    type Body = Full<Bytes>;
    type BodyError = Infallible;

    fn into_response(self) -> Response<Self::Body> {
        let mut res = Response::new(self.0.into());
        res.headers_mut()
            .insert(header::CONTENT_TYPE, HeaderValue::from_static("text/html; charset=utf-8"));
        res
    }
}

impl<T> From<T> for Utf8Html<T> {
    fn from(inner: T) -> Self {
        Self(inner)
    }
}
