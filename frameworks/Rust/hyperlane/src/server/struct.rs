use super::*;

#[allow(bad_style)]
#[derive(Serialize, Default, Clone)]
pub struct QueryRow {
    pub id: Queries,
    pub randomNumber: Queries,
}

#[derive(Serialize)]
pub struct Fortunes {
    pub id: Queries,
    pub message: String,
}

#[derive(Serialize)]
pub struct FortunesTemplate(pub Vec<Fortunes>);
