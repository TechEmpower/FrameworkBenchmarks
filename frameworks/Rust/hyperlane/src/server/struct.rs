use super::*;

#[allow(bad_style)]
#[derive(Clone, Copy, Default, Serialize)]
pub(crate) struct QueryRow {
    pub(crate) id: Queries,
    pub(crate) randomNumber: Queries,
}

#[derive(Clone, Default, Serialize)]
pub(crate) struct Fortunes {
    pub(crate) id: Queries,
    pub(crate) message: String,
}

#[derive(Clone, Default, Serialize)]
pub(crate) struct FortunesTemplate(pub(crate) Vec<Fortunes>);
