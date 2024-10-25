use axum::extract::rejection::JsonRejection::MissingJsonContentType;
use axum::extract::Request;
use axum::extract::{rejection::*, FromRequest};
use axum::{async_trait, http};
use axum_core::response::{IntoResponse, Response};
use bytes::{BufMut, Bytes, BytesMut};
use http::{
    header::{self, HeaderMap, HeaderValue},
    StatusCode,
};
use serde::{de::DeserializeOwned, Serialize};
use simd_json;

#[derive(Debug, Clone, Copy, Default)]
pub struct Json<T>(pub T);

pub enum SimdJsonRejection {
    Json(JsonRejection),
    Bytes(BytesRejection),
    Simd(String),
}

impl IntoResponse for SimdJsonRejection {
    fn into_response(self) -> Response {
        todo!()
    }
}

impl From<JsonRejection> for SimdJsonRejection {
    fn from(err: JsonRejection) -> Self {
        SimdJsonRejection::Json(err)
    }
}

impl From<BytesRejection> for SimdJsonRejection {
    fn from(err: BytesRejection) -> Self {
        SimdJsonRejection::Bytes(err)
    }
}

impl From<simd_json::Error> for SimdJsonRejection {
    fn from(err: simd_json::Error) -> Self {
        SimdJsonRejection::Simd(err.to_string())
    }
}

#[async_trait]
impl<T, S> FromRequest<S> for Json<T>
where
    T: DeserializeOwned,
    S: Send + Sync,
{
    type Rejection = SimdJsonRejection;

    async fn from_request(req: Request, state: &S) -> Result<Self, Self::Rejection> {
        if json_content_type(req.headers()) {
            let bytes = Bytes::from_request(req, state).await?;
            Self::from_bytes(&bytes)
        } else {
            Err(SimdJsonRejection::Json(MissingJsonContentType(
                axum::extract::rejection::MissingJsonContentType::default(),
            )))
        }
    }
}

fn json_content_type(headers: &HeaderMap) -> bool {
    let content_type = if let Some(content_type) = headers.get(header::CONTENT_TYPE) {
        content_type
    } else {
        return false;
    };

    let content_type = if let Ok(content_type) = content_type.to_str() {
        content_type
    } else {
        return false;
    };

    let mime = if let Ok(mime) = content_type.parse::<mime::Mime>() {
        mime
    } else {
        return false;
    };

    let is_json_content_type = mime.type_() == "application"
        && (mime.subtype() == "json"
            || mime.suffix().map_or(false, |name| name == "json"));

    is_json_content_type
}

axum_core::__impl_deref!(Json);

impl<T> From<T> for Json<T> {
    fn from(inner: T) -> Self {
        Self(inner)
    }
}

impl<T> Json<T>
where
    T: DeserializeOwned,
{
    /// Construct a `Json<T>` from a byte slice. Most users should prefer to use the `FromRequest` impl
    /// but special cases may require first extracting a `Request` into `Bytes` then optionally
    /// constructing a `Json<T>`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SimdJsonRejection> {
        let body = &mut bytes.to_owned();
        let deserializer = simd_json::from_slice::<T>(body);

        let value = match deserializer {
            Ok(v) => v,
            Err(err) => {
                let rejection = { SimdJsonRejection::from(err) };
                return Err(rejection);
            }
        };

        Ok(Json(value))
    }
}

impl<T> IntoResponse for Json<T>
where
    T: Serialize,
{
    fn into_response(self) -> Response {
        // Use a small initial capacity of 128 bytes like serde_json::to_vec
        // https://docs.rs/serde_json/1.0.82/src/serde_json/ser.rs.html#2189
        let mut buf = BytesMut::with_capacity(128).writer();
        match simd_json::to_writer(&mut buf, &self.0) {
            Ok(()) => (
                [(
                    header::CONTENT_TYPE,
                    HeaderValue::from_static(mime::APPLICATION_JSON.as_ref()),
                )],
                buf.into_inner().freeze(),
            )
                .into_response(),
            Err(err) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                [(
                    header::CONTENT_TYPE,
                    HeaderValue::from_static(mime::TEXT_PLAIN_UTF_8.as_ref()),
                )],
                err.to_string(),
            )
                .into_response(),
        }
    }
}
