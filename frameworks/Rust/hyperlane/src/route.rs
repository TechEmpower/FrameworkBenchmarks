use crate::*;

#[inline]
pub async fn json(controller_data: ControllerData) {
    let json: serde_json::Value = json!({
        "message": RESPONSEDATA
    });
    let _ = controller_data
        .set_response_body(serde_json::to_string(&json).unwrap_or_default())
        .await;
}

#[inline]
pub async fn plaintext(controller_data: ControllerData) {
    let _ = controller_data
        .set_response_header(CONTENT_TYPE, TEXT_PLAIN)
        .await
        .set_response_body(RESPONSEDATA)
        .await;
}

#[inline]
pub async fn db(controller_data: ControllerData) {
    let db_connection: DbPoolConnection = get_db_connection().await;
    let query_row: QueryRow = random_world_row(&db_connection).await.unwrap();
    let _ = controller_data
        .set_response_body(serde_json::to_string(&query_row).unwrap_or_default())
        .await;
}

#[inline]
pub async fn queries(controller_data: ControllerData) {
    let queries: Queries = controller_data
        .get_request_query("q")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let mut data: Vec<QueryRow> = Vec::with_capacity(queries as usize);
    let db_pool: DbPoolConnection = get_db_connection().await;
    for _ in 0..queries {
        let _ = random_world_row(&db_pool).await.map(|row| {
            data.push(row);
        });
    }
    let _ = controller_data
        .set_response_body(serde_json::to_string(&data).unwrap_or_default())
        .await;
}

markup::define! {
    FortunesTemplate(fortunes: Vec<Fortunes>) {
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

#[inline]
pub async fn fortunes(controller_data: ControllerData) {
    let all_rows: Vec<PgRow> = all_world_row().await;
    let mut fortunes_list: Vec<Fortunes> = all_rows
        .iter()
        .map(|row| {
            let id: i32 = row.get(KEY_ID);
            let message: String = row.get(KEY_RANDOM_NUMBER);
            Fortunes::new(id, message)
        })
        .collect();
    fortunes_list.push(Fortunes::new(
        0,
        "Additional fortune added at request time.".to_owned(),
    ));
    fortunes_list.sort_by(|it, next| it.message.cmp(&next.message));
    let mut result: String = String::with_capacity(2048);
    let _ = write!(
        &mut result,
        "{}",
        FortunesTemplate {
            fortunes: fortunes_list
        }
    );
    controller_data
        .set_response_header(CONTENT_TYPE, format!("{}; {}", TEXT_HTML, CHARSET_UTF_8))
        .await
        .set_response_body(result)
        .await;
}

#[inline]
pub async fn updates(controller_data: ControllerData) {
    let queries: Queries = controller_data
        .get_request_query("q")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let res: Vec<QueryRow> = update_world_rows(queries).await.unwrap();
    let _ = controller_data
        .set_response_body(serde_json::to_string(&res).unwrap_or_default())
        .await;
}
