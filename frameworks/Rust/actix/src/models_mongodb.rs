use serde::{Deserialize, Serialize};

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct World {
    pub id: i32,
    pub random_number: i32,
}

// The ids are stored in MongoDB as floating point numbers, so need a custom deserialization implementation
// to handle converting them.
impl<'de> Deserialize<'de> for World {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(rename_all = "camelCase")]
        struct FloatWorld {
            id: f32,
            random_number: f32,
        }

        let float = FloatWorld::deserialize(deserializer)?;
        Ok(World {
            id: float.id as i32,
            random_number: float.random_number as i32,
        })
    }
}

#[allow(non_snake_case)]
#[derive(Serialize, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

// The ids are stored in MongoDB as floating point numbers, so need a custom deserialization implementation
// to handle converting them.
impl<'de> Deserialize<'de> for Fortune {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct FloatFortune {
            id: f32,
            message: String,
        }

        let float = FloatFortune::deserialize(deserializer)?;
        Ok(Fortune {
            id: float.id as i32,
            message: float.message,
        })
    }
}

impl Default for Fortune {
    fn default() -> Self {
        Fortune {
            id: -1,
            message: "".to_string(),
        }
    }
}
