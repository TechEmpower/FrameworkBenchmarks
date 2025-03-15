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

#[inline]
pub async fn fortunes(controller_data: ControllerData) {
    let all_rows: Vec<PgRow> = all_world_row().await;
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
    let template: &str = include_str!("../templates/fortune.hbs");
    let mut handlebars: Handlebars<'_> = Handlebars::new();
    handlebars
        .register_template_string("fortunes", template)
        .unwrap();
    let mut data: HashMap<&str, Vec<Fortunes>> = HashMap::new();
    data.insert("fortunes", fortunes_list);
    let result: String = handlebars.render("fortunes", &data).unwrap();
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
