use crate::*;

pub async fn json(controller_data: ControllerData) {
    let json: Value = json!({
        "message": RESPONSEDATA_STR
    });
    let _ = controller_data
        .set_response_body(serde_json::to_string(&json).unwrap_or_default())
        .await;
}

pub async fn plaintext(controller_data: ControllerData) {
    let _ = controller_data
        .set_response_header(CONTENT_TYPE, TEXT_PLAIN)
        .await
        .set_response_body(RESPONSEDATA_BIN)
        .await;
}

pub async fn db(controller_data: ControllerData) {
    let db_connection: DbPoolConnection = get_db_connection().await;
    let query_row: QueryRow = random_world_row(&db_connection).await;
    let _ = controller_data
        .set_response_body(serde_json::to_string(&query_row).unwrap_or_default())
        .await;
}

pub async fn queries(controller_data: ControllerData) {
    let queries: Queries = controller_data
        .get_request_query("q")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let db_pool: DbPoolConnection = get_db_connection().await;
    let data: Vec<QueryRow> = get_some_row_id(queries, &db_pool).await;
    let _ = controller_data
        .set_response_body(serde_json::to_string(&data).unwrap_or_default())
        .await;
}

pub async fn fortunes(controller_data: ControllerData) {
    let all_rows: Vec<PgRow> = all_world_row().await;
    let mut fortunes_list: Vec<Fortunes> = all_rows
        .iter()
        .map(|row| {
            let id: i32 = row.get(KEY_ID);
            let message: String = row.get(KEY_MESSAGE);
            Fortunes::new(id, message)
        })
        .collect();
    fortunes_list.push(Fortunes::new(
        0,
        "Additional fortune added at request time.".to_owned(),
    ));
    fortunes_list.sort_by(|it, next| it.message.cmp(&next.message));
    let res: String = FortunesTemplate::new(fortunes_list).to_string();
    controller_data
        .set_response_header(CONTENT_TYPE, content_type_charset(TEXT_HTML, UTF8))
        .await
        .set_response_body(res)
        .await;
}

pub async fn updates(controller_data: ControllerData) {
    let queries: Queries = controller_data
        .get_request_query("q")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let res: Vec<QueryRow> = update_world_rows(queries).await;
    let _ = controller_data
        .set_response_body(serde_json::to_string(&res).unwrap_or_default())
        .await;
}

pub async fn cached_queries(controller_data: ControllerData) {
    let count: Queries = controller_data
        .get_request_query("c")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let res: Vec<QueryRow> = CACHE
        .get()
        .unwrap_or(&Vec::new())
        .iter()
        .take(count as usize)
        .cloned()
        .collect();
    let _ = controller_data
        .set_response_body(serde_json::to_string(&res).unwrap_or_default())
        .await;
}
