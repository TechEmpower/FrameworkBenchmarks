//! light weight proc macro are manually expanded to reduce compile time resource usage
//! there is no runtime perf difference between manual expansion and deault code generation

use serde_core::{Serialize, Serializer, ser::SerializeStruct};

#[cfg(feature = "pg")]
use xitca_unsafe_collection::bytes::BytesStr;

use crate::util::HandleResult;

#[cfg_attr(feature = "diesel", derive(diesel::Queryable))]
#[cfg_attr(feature = "toasty", derive(toasty::Model), table = "world")]
pub struct World {
    #[cfg_attr(feature = "toasty", key)]
    pub id: i32,
    pub randomnumber: i32,
}

#[cfg(not(feature = "diesel"))]
impl World {
    #[inline]
    pub const fn new(id: i32, randomnumber: i32) -> Self {
        Self { id, randomnumber }
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

#[cfg_attr(feature = "diesel", derive(diesel::Queryable))]
#[cfg_attr(feature = "toasty", derive(toasty::Model), table = "fortune")]
pub struct Fortune {
    #[cfg_attr(feature = "toasty", key)]
    pub id: i32,
    pub message: FortuneMessage,
}

#[cfg(feature = "zero-copy")]
type FortuneMessage = BytesStr;

#[cfg(not(feature = "zero-copy"))]
type FortuneMessage = String;

impl Fortune {
    const RUNTIME: &str = "Additional fortune added at request time.";

    #[cfg(feature = "zero-copy")]
    fn runtime() -> Self {
        Self::new(0, const { BytesStr::from_static(Self::RUNTIME) })
    }

    #[cfg(not(feature = "zero-copy"))]
    #[inline]
    fn runtime() -> Self {
        Self::new(0, String::from(Self::RUNTIME))
    }

    #[inline]
    pub const fn new(id: i32, message: FortuneMessage) -> Self {
        Self { id, message }
    }
}

pub struct Fortunes {
    items: Vec<Fortune>,
}

impl Fortunes {
    #[inline]
    pub fn new(mut items: Vec<Fortune>) -> Self {
        items.push(Fortune::runtime());
        items.sort_by(|a, b| a.message.cmp(&b.message));
        Self { items }
    }

    #[cfg(feature = "template")]
    pub fn render_once(self) -> HandleResult<String> {
        use sailfish::runtime::{Buffer, Render};

        const PREFIX: &str = "<!DOCTYPE html>\n<html>\n<head><title>Fortunes</title></head>\n<body>\n<table>\n<tr><th>id</th><th>message</th></tr>\n";
        const SUFFIX: &str = "\n</table>\n</body>\n</html>";

        let mut buf = Buffer::with_capacity(1236);

        buf.push_str(PREFIX);
        for item in self.items {
            buf.push_str("<tr><td>");
            Render::render_escaped(&item.id, &mut buf)?;
            buf.push_str("</td><td>");
            Render::render_escaped(item.message.as_str(), &mut buf)?;
            buf.push_str("</td></tr>");
        }
        buf.push_str(SUFFIX);

        Ok(buf.into_string())
    }
}

pub const HELLO: &str = "Hello, World!";

#[derive(Clone)]
pub struct Message {
    message: &'static str,
}

impl Message {
    pub const HELLO: &Self = &Self { message: HELLO };
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

#[cfg(feature = "web-codegen")]
pub use num::Num;

#[cfg(feature = "web-codegen")]
mod num {
    use serde_core::{Deserialize, Deserializer};

    pub struct Num(pub u16);

    impl<'de> Deserialize<'de> for Num {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            use core::fmt;

            use serde_core::de::{Error, MapAccess, Visitor};

            const FIELDS: &[&str] = &["q"];

            struct Field;

            impl<'de> Deserialize<'de> for Field {
                fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
                where
                    D: Deserializer<'de>,
                {
                    struct FieldVisitor;

                    impl Visitor<'_> for FieldVisitor {
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
}

#[cfg(all(feature = "perf-json", not(feature = "json")))]
pub use sonic_rs::to_vec as json_serialize;

#[cfg(all(feature = "json", not(feature = "perf-json")))]
pub use serde_json::to_vec as json_serialize;
