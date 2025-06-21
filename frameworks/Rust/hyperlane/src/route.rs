use super::*;

pub async fn json(ctx: Context) {
    let json: Value = json!({
        "message": RESPONSEDATA_STR
    });
    let _ = ctx
        .set_response_body(serde_json::to_string(&json).unwrap_or_default())
        .await
        .send()
        .await;
}

pub async fn plaintext(ctx: Context) {
    let _ = ctx.set_response_body(RESPONSEDATA_BIN).await.send().await;
}

pub async fn db(ctx: Context) {
    let db_connection: &DbPoolConnection = get_db_connection();
    let query_row: QueryRow = random_world_row(db_connection).await;
    let _ = ctx
        .set_response_body(serde_json::to_string(&query_row).unwrap_or_default())
        .await
        .send()
        .await;
}

pub async fn query(ctx: Context) {
    let queries: Queries = ctx
        .get_request_query("q")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let db_pool: &DbPoolConnection = get_db_connection();
    let data: Vec<QueryRow> = get_some_row_id(queries, db_pool).await;
    let _ = ctx
        .set_response_body(serde_json::to_string(&data).unwrap_or_default())
        .await
        .send()
        .await;
}

pub async fn fortunes(ctx: Context) {
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
    let _ = ctx.set_response_body(res).await.send().await;
}

pub async fn update(ctx: Context) {
    let queries: Queries = ctx
        .get_request_query("q")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let res: Vec<QueryRow> = update_world_rows(queries).await;
    let _ = ctx
        .set_response_body(serde_json::to_string(&res).unwrap_or_default())
        .await
        .send()
        .await;
}

pub async fn cached_query(ctx: Context) {
    let count: Queries = ctx
        .get_request_query("c")
        .await
        .and_then(|queries| queries.parse::<Queries>().ok())
        .unwrap_or_default()
        .min(ROW_LIMIT as Queries)
        .max(1);
    let res: Vec<QueryRow> = CACHE.iter().take(count as usize).cloned().collect();
    let _ = ctx
        .set_response_body(serde_json::to_string(&res).unwrap_or_default())
        .await
        .send()
        .await;
}
