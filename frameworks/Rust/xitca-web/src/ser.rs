#![allow(dead_code)]

use std::borrow::Cow;

use serde::{ser::SerializeStruct, Deserialize, Deserializer, Serialize, Serializer};
use xitca_http::{
    body::Once,
    bytes::{BufMutWriter, Bytes, BytesMut},
    http::{const_header_value::JSON, header::CONTENT_TYPE, IntoResponse, Request, Response},
};

use crate::util::Error;

#[derive(Clone)]
pub struct Message {
    message: &'static str,
}

impl Message {
    #[inline]
    pub const fn new() -> Self {
        Self {
            message: "Hello, World!",
        }
    }
}

pub struct Num(pub u16);

#[cfg_attr(feature = "pg-orm", derive(diesel::Queryable))]
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

#[cfg_attr(feature = "pg-orm", derive(diesel::Queryable))]
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

// TODO: use another template engine with faster compile time.(perferably with no proc macro)
#[cfg_attr(
    feature = "template",
    derive(sailfish::TemplateOnce),
    template(path = "fortune.stpl", rm_whitespace = true)
)]
pub struct Fortunes {
    items: Vec<Fortune>,
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
        use core::{cmp, fmt};

        use serde::de::{Error, MapAccess, Visitor};

        const FIELDS: &'static [&'static str] = &["q"];

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
                map.next_key::<Field>()?
                    .ok_or_else(|| Error::missing_field("q"))?;
                let q = map.next_value::<u16>().unwrap_or(1);
                let q = cmp::min(500, cmp::max(1, q));
                Ok(Num(q))
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

pub fn json_response<Ext, S>(
    req: Request<Ext>,
    buf: &mut BytesMut,
    value: &S,
) -> Result<Response<Once<Bytes>>, Error>
where
    S: ?Sized + Serialize,
{
    serde_json::to_writer(BufMutWriter(buf), value)?;
    let mut res = req.into_response(buf.split().freeze());
    res.headers_mut().insert(CONTENT_TYPE, JSON);
    Ok(res)
}
