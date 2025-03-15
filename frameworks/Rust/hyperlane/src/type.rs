use crate::*;

pub type DbPoolConnection = Pool<PostgresConnectionManager<NoTls>>;
pub type DbConnection<'a> = PooledConnection<'a, PostgresConnectionManager<NoTls>>;
pub type Queries = i32;

#[allow(bad_style)]
#[derive(Serialize, Default, Clone)]
pub struct QueryRow {
    pub id: i32,
    pub randomNumber: i32,
}

impl QueryRow {
    #[inline]
    pub fn new(id: i32, random_number: i32) -> Self {
        Self {
            id,
            randomNumber: random_number,
        }
    }
}

#[derive(Serialize)]
pub struct Fortunes {
    pub id: i32,
    pub message: String,
}

impl Fortunes {
    #[inline]
    pub fn new(id: i32, message: String) -> Self {
        Self { id, message }
    }
}

pub struct FortunesTemplate {
    pub fortunes_list: Vec<Fortunes>,
}

impl FortunesTemplate {
    pub fn new(fortunes_list: Vec<Fortunes>) -> Self {
        Self { fortunes_list }
    }
}

impl fmt::Display for FortunesTemplate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>")?;
        for fortunes in &self.fortunes_list {
            let decoded_message: Cow<'_, str> = encode_text(&fortunes.message);
            write!(
                f,
                "<tr><td>{}</td><td>{}</td></tr>",
                fortunes.id, decoded_message
            )?;
        }
        write!(f, "</table></body></html>")
    }
}
