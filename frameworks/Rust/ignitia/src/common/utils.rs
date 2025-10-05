use serde::Deserialize;

#[allow(dead_code)]
#[derive(Debug, Deserialize)]
pub struct Params {
    #[serde(default = "default_q")]
    pub q: usize,
}

fn default_q() -> usize {
    1
}

#[allow(dead_code)]
#[inline(always)]
pub fn parse_params(params: Params) -> usize {
    params.q.clamp(1, 500)
}
