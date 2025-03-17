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
    let query_row: QueryRow = random_world_row(&db_connection).await;
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
        let row: QueryRow = random_world_row(&db_pool).await;
        data.push(row);
    }
    let _ = controller_data
        .set_response_body(serde_json::to_string(&data).unwrap_or_default())
        .await;
}

#[inline]
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

#[inline]
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

#[inline]
pub async fn cached_queries(controller_data: ControllerData) {
    let count: Queries = controller_data
        .get_request_query("c")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let mut res: Vec<QueryRow> = Vec::with_capacity(count as usize);
    let cache: RwLockReadGuard<'_, Vec<QueryRow>> = CACHE.read().await;
    for i in 0..count {
        res.push(cache[i as usize].clone());
    }
    let _ = controller_data
        .set_response_body(serde_json::to_string(&res).unwrap_or_default())
        .await;
}
