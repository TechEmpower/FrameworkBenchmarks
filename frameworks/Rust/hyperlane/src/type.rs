use crate::*;

pub type DbPoolConnection = Pool<Postgres>;
pub type Queries = i32;

#[allow(bad_style)]
#[derive(Serialize, Default, Clone)]
pub struct QueryRow {
    pub id: Queries,
    pub randomNumber: Queries,
}

impl QueryRow {
    pub fn new(id: Queries, random_number: Queries) -> Self {
        Self {
            id,
            randomNumber: random_number,
        }
    }
}

#[derive(Serialize)]
pub struct Fortunes {
    pub id: Queries,
    pub message: String,
}

impl Fortunes {
    pub fn new(id: Queries, message: String) -> Self {
        Self { id, message }
    }
}

#[derive(Serialize)]
pub struct FortunesTemplate(pub Vec<Fortunes>);

impl FortunesTemplate {
    pub fn new(list: Vec<Fortunes>) -> Self {
        Self(list)
    }
}

impl fmt::Display for FortunesTemplate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let fortunes: &Vec<Fortunes> = &self.0;
        let _ = write!(
            f,
            "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
        );
        for tem in fortunes.iter() {
            let row: String = format!(
                "<tr><td>{}</td><td>{}</td></tr>",
                tem.id,
                escape_html(&tem.message)
            );
            let _ = write!(f, "{}", row);
        }
        let _ = write!(f, "</table></body></html>");
        Ok(())
    }
}
