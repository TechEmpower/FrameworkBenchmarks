#![allow(dead_code)]

use std::borrow::Cow;

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

#[cfg_attr(feature = "pg-orm", derive(Queryable))]
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

#[cfg_attr(feature = "pg-orm", derive(Queryable))]
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

#[cfg(feature = "serde")]
pub use _serde::*;

#[cfg(feature = "serde")]
mod _serde {
    use serde::{ser::SerializeStruct, Serialize, Serializer};
    use xitca_http::{
        body::Once,
        bytes::{BufMutWriter, Bytes, BytesMut},
        http::{const_header_value::JSON, header::CONTENT_TYPE, IntoResponse, Request, Response},
    };

    use crate::util::Error;

    use super::{Message, World};

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
        res.headers_mut().append(CONTENT_TYPE, JSON);
        Ok(res)
    }
}
