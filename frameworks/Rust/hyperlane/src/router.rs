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
        .set_request_header(CONTENT_TYPE, TEXT_PLAIN)
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
        .get_request_query("queries")
        .await
        .map(|queries| queries.parse::<Queries>().unwrap_or_default())
        .unwrap_or(0)
        .min(ROW_LIMIT as usize);
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
