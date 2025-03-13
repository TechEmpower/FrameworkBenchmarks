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
        .min(ROW_LIMIT as usize)
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
