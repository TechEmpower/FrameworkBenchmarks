use super::*;

#[allow(bad_style)]
#[derive(Serialize, Default, Clone)]
pub(crate) struct QueryRow {
    pub(crate) id: Queries,
    pub(crate) randomNumber: Queries,
}

#[derive(Serialize)]
pub(crate) struct Fortunes {
    pub(crate) id: Queries,
    pub(crate) message: String,
}

#[derive(Serialize)]
pub(crate) struct FortunesTemplate(pub(crate) Vec<Fortunes>);
