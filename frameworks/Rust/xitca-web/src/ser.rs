#![allow(dead_code)]

use std::borrow::Cow;

use serde::{ser::SerializeStruct, Deserialize, Deserializer, Serialize, Serializer};
use xitca_http::{
    body::Once,
    bytes::{BufMutWriter, Bytes},
    http::{
        self,
        const_header_value::{JSON, TEXT_HTML_UTF8, TEXT_UTF8},
        header::CONTENT_TYPE,
        IntoResponse as _, RequestExt, StatusCode,
    },
};

use crate::util::{Error, State};

const HELLO: &str = "Hello, World!";
const HELLO_BYTES: &[u8] = HELLO.as_bytes();

#[cfg_attr(feature = "perf", derive(simd_json_derive::Serialize))]
#[derive(Clone)]
pub struct Message {
    message: &'static str,
}

impl Message {
    #[inline]
    pub const fn new() -> Self {
        Self { message: HELLO }
    }
}

pub struct Num(pub u16);

#[cfg_attr(any(feature = "pg-orm", feature = "pg-orm-async"), derive(diesel::Queryable))]
#[cfg_attr(feature = "perf", derive(simd_json_derive::Serialize))]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

impl World {
    #[inline]
    pub const fn new(id: i32, randomnumber: i32) -> Self {
        Self { id, randomnumber }
    }
}

#[cfg_attr(any(feature = "pg-orm", feature = "pg-orm-async"), derive(diesel::Queryable))]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

impl Fortune {
    #[inline]
    pub fn new(id: i32, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            id,
            message: message.into(),
        }
    }
}

pub struct Fortunes {
    items: Vec<Fortune>,
}

// this is roughly the code generated by sailfish::TemplateOnce macro.
// using the macro does not have any perf cost and this piece of code is expanded manually to speed up compile time of
// bench to reduce resource usage of bench runner
#[cfg(feature = "template")]
impl sailfish::TemplateOnce for Fortunes {
    fn render_once(self) -> sailfish::RenderResult {
        use sailfish::runtime::{Buffer, Render};

        const PREFIX: &str = "<!DOCTYPE html>\n<html>\n<head><title>Fortunes</title></head>\n<body>\n<table>\n<tr><th>id</th><th>message</th></tr>\n";
        const SUFFIX: &str = "\n</table>\n</body>\n</html>";

        let mut buf = Buffer::with_capacity(1236);

        buf.push_str(PREFIX);
        for item in self.items {
            buf.push_str("<tr><td>");
            Render::render_escaped(&item.id, &mut buf)?;
            buf.push_str("</td><td>");
            Render::render_escaped(&item.message, &mut buf)?;
            buf.push_str("</td></tr>");
        }
        buf.push_str(SUFFIX);

        Ok(buf.into_string())
    }

    fn render_once_to(self, _: &mut sailfish::runtime::Buffer) -> Result<(), sailfish::runtime::RenderError> {
        unimplemented!("")
    }
}

impl Fortunes {
    #[inline]
    pub const fn new(items: Vec<Fortune>) -> Self {
        Self { items }
    }
}

impl<'de> Deserialize<'de> for Num {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use core::fmt;

        use serde::de::{Error, MapAccess, Visitor};

        const FIELDS: &[&str] = &["q"];

        struct Field;

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct FieldVisitor;

                impl<'de> Visitor<'de> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("`q`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Field, E>
                    where
                        E: Error,
                    {
                        match value {
                            "q" => Ok(Field),
                            _ => Err(Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        struct NumVisitor;

        impl<'de> Visitor<'de> for NumVisitor {
            type Value = Num;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Num")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Num, V::Error>
            where
                V: MapAccess<'de>,
            {
                map.next_key::<Field>()?.ok_or_else(|| Error::missing_field("q"))?;
                Ok(Num(map.next_value().unwrap_or(1).clamp(1, 500)))
            }
        }

        deserializer.deserialize_struct("Num", FIELDS, NumVisitor)
    }
}

impl Serialize for Message {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut res = serializer.serialize_struct("Message", 1)?;
        res.serialize_field("message", self.message)?;
        res.end()
    }
}

impl Serialize for World {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut res = serializer.serialize_struct("World", 2)?;
        res.serialize_field("id", &self.id)?;
        res.serialize_field("randomnumber", &self.randomnumber)?;
        res.end()
    }
}

pub type Request<B> = http::Request<RequestExt<B>>;
pub type Response = http::Response<Once<Bytes>>;

pub trait IntoResponse: Sized {
    fn json_response<C>(self, state: &State<C>, val: &impl Serialize) -> Result<Response, Error>;

    fn text_response(self) -> Result<Response, Error>;

    fn html_response(self, val: String) -> Result<Response, Error>;
}

impl<Ext> IntoResponse for Request<Ext> {
    fn json_response<C>(self, state: &State<C>, val: &impl Serialize) -> Result<Response, Error> {
        let buf = &mut *state.write_buf.borrow_mut();
        serde_json::to_writer(BufMutWriter(buf), val)?;
        let mut res = self.into_response(buf.split().freeze());
        res.headers_mut().insert(CONTENT_TYPE, JSON);
        Ok(res)
    }

    fn text_response(self) -> Result<Response, Error> {
        let mut res = self.into_response(const { Bytes::from_static(HELLO_BYTES) });
        res.headers_mut().insert(CONTENT_TYPE, TEXT_UTF8);
        Ok(res)
    }

    fn html_response(self, val: String) -> Result<Response, Error> {
        let mut res = self.into_response(Bytes::from(val));
        res.headers_mut().insert(CONTENT_TYPE, TEXT_HTML_UTF8);
        Ok(res)
    }
}

pub fn error_response(status: StatusCode) -> Response {
    http::Response::builder()
        .status(status)
        .body(Once::new(Bytes::new()))
        .unwrap()
}
