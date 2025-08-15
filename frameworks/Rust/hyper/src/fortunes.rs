
use http::header::{CONTENT_LENGTH, CONTENT_TYPE};
use http::Response;
use http_body_util::Full;
use hyper::body::Bytes;
use tokio_postgres::Row;

use crate::db::POOL;
use crate::{Error, Result, TEXT_HTML};

const QUERY: &str = "SELECT id, message FROM fortune";

pub struct Fortune {
    pub id: i32,
    pub message: String,
}

impl From<&Row> for Fortune {
    fn from(row: &Row) -> Self {
        Self {
            id: row.get(0),
            message: row.get(1),
        }
    }
}

pub async fn get() -> Result<Response<Full<Bytes>>> {
    let fortunes = tell_fortune().await?;
    let content = FortunesTemplate { fortunes }.to_string();
    
    Response::builder()
        .header(CONTENT_TYPE, TEXT_HTML.clone())
        .header(CONTENT_LENGTH, content.len())
        .body(content.into())
        .map_err(Error::from)
}

async fn tell_fortune() -> Result<Vec<Fortune>> {
    let db = POOL.get().await?;
    let statement = db.prepare_cached(QUERY).await?;
    let rows = db.query(&statement, &[]).await?;
    let mut fortunes = rows.iter().map(Fortune::from).collect::<Vec<_>>();

    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });
    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

    Ok(fortunes)
}

markup::define! {
    FortunesTemplate(fortunes: Vec<Fortune>) {
        {markup::doctype()}
        html {
            head {
                title { "Fortunes" }
            }
            body {
                table {
                    tr { th { "id" } th { "message" } }
                    @for item in fortunes {
                        tr {
                            td { {item.id} }
                            td { {markup::raw(v_htmlescape::escape(&item.message))} }
                        }
                    }
                }
            }
        }
    }
}
