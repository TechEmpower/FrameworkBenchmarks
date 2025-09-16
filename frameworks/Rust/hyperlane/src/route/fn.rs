use super::*;

pub async fn json(ctx: Context) {
    let json: Value = json!({
        "message": RESPONSEDATA_STR
    });
    let run = || async {
        ctx.set_response_body(&serde_json::to_vec(&json).unwrap_or_default())
            .await;
        ctx.send().await.unwrap();
    };
    run().await;
    while let Ok(_) = ctx.http_from_stream(HTTP_BUFFER).await {
        run().await;
    }
    ctx.closed().await;
}

pub async fn plaintext(ctx: Context) {
    ctx.set_response_header(CONTENT_TYPE, TEXT_PLAIN).await;
    ctx.set_response_body(&RESPONSEDATA_BIN).await;
    let run = || async {
        ctx.send().await.unwrap();
    };
    run().await;
    while let Ok(_) = ctx.http_from_stream(HTTP_BUFFER).await {
        run().await;
    }
    ctx.closed().await;
}

pub async fn db(ctx: Context) {
    let db_connection: &DbPoolConnection = get_db_connection();
    let run = || async {
        let query_row: QueryRow = random_world_row(db_connection).await;
        ctx.set_response_body(&serde_json::to_vec(&query_row).unwrap_or_default())
            .await
            .send()
            .await
            .unwrap();
    };
    run().await;
    while let Ok(_) = ctx.http_from_stream(HTTP_BUFFER).await {
        run().await;
    }
    ctx.closed().await;
}

pub async fn query(ctx: Context) {
    let run = || async {
        let queries: Queries = ctx
            .try_get_request_query(QUERY_DB_QUERY_KEY)
            .await
            .and_then(|queries| queries.parse::<Queries>().ok())
            .unwrap_or_default()
            .min(ROW_LIMIT as Queries)
            .max(1);
        let db_pool: &DbPoolConnection = get_db_connection();
        let data: Vec<QueryRow> = get_some_row_id(queries, db_pool).await;
        ctx.set_response_body(&serde_json::to_vec(&data).unwrap_or_default())
            .await
            .send()
            .await
            .unwrap();
    };
    run().await;
    while let Ok(_) = ctx.http_from_stream(HTTP_BUFFER).await {
        run().await;
    }
    ctx.closed().await;
}

pub async fn fortunes(ctx: Context) {
    ctx.set_response_header(
        CONTENT_TYPE,
        &ContentType::format_content_type_with_charset(TEXT_HTML, UTF8),
    )
    .await;
    let run = || async {
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
        ctx.set_response_body(&res).await.send().await.unwrap();
    };
    run().await;
    while let Ok(_) = ctx.http_from_stream(HTTP_BUFFER).await {
        run().await;
    }
    ctx.closed().await;
}

pub async fn update(ctx: Context) {
    let run = || async {
        let queries: Queries = ctx
            .try_get_request_query(UPDATE_DB_QUERY_KEY)
            .await
            .and_then(|queries| queries.parse::<Queries>().ok())
            .unwrap_or_default()
            .min(ROW_LIMIT as Queries)
            .max(1);
        let res: Vec<QueryRow> = update_world_rows(queries).await;
        ctx.set_response_body(&serde_json::to_vec(&res).unwrap_or_default())
            .await
            .send()
            .await
            .unwrap();
    };
    run().await;
    while let Ok(_) = ctx.http_from_stream(HTTP_BUFFER).await {
        run().await;
    }
    ctx.closed().await;
}

pub async fn cached_query(ctx: Context) {
    let run = || async {
        let count: Queries = ctx
            .try_get_request_query(CACHE_QUERY_KEY)
            .await
            .and_then(|queries| queries.parse::<Queries>().ok())
            .unwrap_or_default()
            .min(ROW_LIMIT as Queries)
            .max(1);
        let res: Vec<&QueryRow> = CACHE.iter().take(count as usize).collect();
        ctx.set_response_body(&serde_json::to_vec(&res).unwrap_or_default())
            .await
            .send()
            .await
            .unwrap();
    };
    run().await;
    while let Ok(_) = ctx.http_from_stream(HTTP_BUFFER).await {
        run().await;
    }
    ctx.closed().await;
}
