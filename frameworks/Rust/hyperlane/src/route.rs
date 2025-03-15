use crate::*;

pub async fn json(controller_data: ControllerData) {
    let json: serde_json::Value = json!({
        "message": RESPONSEDATA
    });
    let _ = controller_data
        .set_response_body(serde_json::to_string(&json).unwrap_or_default())
        .await;
}

pub async fn plaintext(controller_data: ControllerData) {
    let _ = controller_data
        .set_response_header(CONTENT_TYPE, TEXT_PLAIN)
        .await
        .set_response_body(RESPONSEDATA)
        .await;
}

pub async fn db(controller_data: ControllerData) {
    let query_row: QueryRow = random_world_row().await.unwrap();
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
    let mut data: Vec<QueryRow> = Vec::with_capacity(queries);
    for _ in 0..queries {
        let _ = random_world_row().await.map(|row| {
            data.push(row);
        });
    }
    let _ = controller_data
        .set_response_body(serde_json::to_string(&data).unwrap_or_default())
        .await;
}

pub async fn fortune(controller_data: ControllerData) {
    let all_rows: Vec<Row> = all_world_row().await.unwrap_or_default();
    let fortune_list: Vec<Fortune> = all_rows
        .iter()
        .map(|row| {
            let id: i32 = row.get(0);
            let message: i32 = row.get(1);
            Fortune::new(id, message)
        })
        .collect();
    let template: &str = include_str!("../templates/fortune.hbs");
    let mut handlebars: Handlebars<'_> = Handlebars::new();
    let _ = handlebars.register_template_string("fortunes", template);
    let res: String = handlebars
        .render("fortunes", &serde_json::json!({ "fortunes": fortune_list }))
        .unwrap_or_default();
    controller_data
        .set_response_header(CONTENT_TYPE, TEXT_HTML)
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
    let res: Vec<QueryRow> = update_world_rows(queries).await.unwrap();
    let _ = controller_data
        .set_response_body(serde_json::to_string(&res).unwrap_or_default())
        .await;
}
