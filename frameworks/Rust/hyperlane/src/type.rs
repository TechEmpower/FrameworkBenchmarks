use crate::*;

pub type DbPoolConnection = bb8::Pool<PostgresConnectionManager<NoTls>>;
pub type DbConnection<'a> = PooledConnection<'a, PostgresConnectionManager<NoTls>>;
pub type Queries = usize;
pub type DynToSqlSync = dyn ToSql + Sync;
pub type DynToSqlSyncSend = dyn ToSql + Sync + Send;

#[allow(bad_style)]
#[derive(Serialize)]
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
            let escaped_message: String = fortunes.message.escape_default().collect::<String>();
            write!(
                f,
                "<tr><td>{}</td><td>{}</td></tr>",
                fortunes.id, escaped_message
            )?;
        }
        write!(f, "</table></body></html>")
    }
}
