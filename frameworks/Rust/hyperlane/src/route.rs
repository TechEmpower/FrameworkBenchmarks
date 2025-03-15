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
    let query_row: QueryRow = random_world_row(&get_db_connection().await).await.unwrap();
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
    let mut data: Vec<QueryRow> = Vec::with_capacity(queries);
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

#[inline]
pub async fn fortunes(controller_data: ControllerData) {
    let all_rows: Vec<Row> = all_world_row().await.unwrap_or_default();
    let mut fortunes_list: Vec<Fortunes> = all_rows
        .iter()
        .map(|row| {
            let id: i32 = row.get(0);
            let message: i32 = row.get(1);
            let message: String = message.to_string();
            Fortunes::new(id, message)
        })
        .collect();
    fortunes_list.push(Fortunes::new(
        0,
        "Additional fortune added at request time.".to_owned(),
    ));
    fortunes_list.sort_by(|a, b| {
        let message_a: &String = &a.message;
        let message_b: &String = &b.message;
        message_a.cmp(message_b)
    });
    let mut res: String = String::with_capacity(20480);
    let _ = write!(&mut res, "{}", FortunesTemplate::new(fortunes_list));
    controller_data
        .set_response_header(CONTENT_TYPE, format!("{}; {}", TEXT_HTML, CHARSET_UTF_8))
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
    let res: Vec<QueryRow> = update_world_rows(queries).await.unwrap();
    let _ = controller_data
        .set_response_body(serde_json::to_string(&res).unwrap_or_default())
        .await;
}
