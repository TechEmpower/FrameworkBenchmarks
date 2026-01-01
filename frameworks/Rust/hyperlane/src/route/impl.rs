use super::*;

impl ServerHook for JsonRoute {
    async fn new(_ctx: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        let json: Value = json!({
            KEY_MESSAGE: RESPONSEDATA_STR
        });
        let run = || async {
            ctx.set_response_body(&serde_json::to_vec(&json).unwrap_or_default())
                .await;
            ctx.send().await.unwrap();
        };
        run().await;
        while ctx.http_from_stream(RequestConfig::default()).await.is_ok() {
            run().await;
        }
        ctx.closed().await;
    }
}

impl ServerHook for PlaintextRoute {
    async fn new(_ctx: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        ctx.set_response_header(CONTENT_TYPE, TEXT_PLAIN).await;
        ctx.set_response_body(&RESPONSEDATA_BIN).await;
        let run = || async {
            ctx.send().await.unwrap();
        };
        run().await;
        while ctx.http_from_stream(RequestConfig::default()).await.is_ok() {
            run().await;
        }
        ctx.closed().await;
    }
}

impl ServerHook for DbRoute {
    async fn new(_ctx: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
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
        while ctx.http_from_stream(RequestConfig::default()).await.is_ok() {
            run().await;
        }
        ctx.closed().await;
    }
}

impl ServerHook for QueryRoute {
    async fn new(_ctx: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
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
        while ctx.http_from_stream(RequestConfig::default()).await.is_ok() {
            run().await;
        }
        ctx.closed().await;
    }
}

impl ServerHook for FortunesRoute {
    async fn new(_ctx: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
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
                    Fortunes::new(id, row.get(KEY_MESSAGE))
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
        while ctx.http_from_stream(RequestConfig::default()).await.is_ok() {
            run().await;
        }
        ctx.closed().await;
    }
}

impl ServerHook for UpdateRoute {
    async fn new(_ctx: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
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
        while ctx.http_from_stream(RequestConfig::default()).await.is_ok() {
            run().await;
        }
        ctx.closed().await;
    }
}

impl ServerHook for CachedQueryRoute {
    async fn new(_ctx: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
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
        while ctx.http_from_stream(RequestConfig::default()).await.is_ok() {
            run().await;
        }
        ctx.closed().await;
    }
}
