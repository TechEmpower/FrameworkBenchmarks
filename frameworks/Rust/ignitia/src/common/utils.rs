use serde::{Deserialize, Deserializer};

#[allow(dead_code)]
#[derive(Debug, Deserialize)]
pub struct Params {
    #[serde(default, deserialize_with = "deserialize_query_count")]
    pub q: usize,
}

// Custom deserializer that handles invalid values
fn deserialize_query_count<'de, D>(deserializer: D) -> Result<usize, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum QueryValue {
        Int(usize),
        String(String),
    }

    let value = Option::<QueryValue>::deserialize(deserializer)?;

    let count = match value {
        Some(QueryValue::Int(i)) => i,
        Some(QueryValue::String(s)) => s.parse().unwrap_or(1),
        None => 1,
    };

    Ok(count.clamp(1, 500))
}

#[allow(dead_code)]
#[inline(always)]
pub fn parse_params(params: Params) -> usize {
    params.q
}
