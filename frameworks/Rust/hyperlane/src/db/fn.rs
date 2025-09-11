use super::*;

pub fn get_db_connection() -> &'static DbPoolConnection {
    &DB
}

#[cfg(feature = "dev")]
pub async fn create_database() {
    let db_pool: &DbPoolConnection = get_db_connection();
    let _ = db_query(&format!("CREATE DATABASE {};", DATABASE_NAME))
        .execute(db_pool)
        .await;
}

#[cfg(feature = "dev")]
pub async fn create_table() {
    let db_pool: &DbPoolConnection = get_db_connection();
    let _ = db_query(&format!(
        "CREATE TABLE IF NOT EXISTS {} (
            id SERIAL PRIMARY KEY, randomNumber INT NOT NULL
        );",
        TABLE_NAME_WORLD
    ))
    .execute(db_pool)
    .await;
    let _ = db_query(&format!(
        "CREATE TABLE IF NOT EXISTS {} (
            id SERIAL PRIMARY KEY, message VARCHAR NOT NULL
        );",
        TABLE_NAME_FORTUNE
    ))
    .execute(db_pool)
    .await;
}

#[cfg(feature = "dev")]
pub async fn insert_records() {
    let db_pool: &DbPoolConnection = get_db_connection();
    let row: PgRow = db_query(&format!("SELECT COUNT(*) FROM {}", TABLE_NAME_WORLD))
        .fetch_one(db_pool)
        .await
        .unwrap();
    let count: i64 = row.get(0);
    let limit: i64 = RANDOM_MAX as i64;
    if count >= limit {
        return;
    }
    let missing_count: i64 = limit - count;
    let mut values: Vec<String> = Vec::new();
    for _ in 0..missing_count {
        let random_number: i32 = get_random_id();
        values.push(format!("(DEFAULT, {})", random_number));
    }
    let sql: String = format!(
        "INSERT INTO {} (id, randomNumber) VALUES {}",
        TABLE_NAME_WORLD,
        values.join(",")
    );
    let _ = db_query(&sql).execute(db_pool).await;
    let mut values: Vec<String> = Vec::new();
    for _ in 0..missing_count {
        let random_number: i32 = get_random_id();
        values.push(format!("(DEFAULT, {})", random_number));
    }
    let sql: String = format!(
        "INSERT INTO {} (id, message) VALUES {}",
        TABLE_NAME_FORTUNE,
        values.join(",")
    );
    let _ = db_query(&sql).execute(db_pool).await;
}

pub async fn init_cache() -> Vec<QueryRow> {
    let mut res: Vec<QueryRow> = Vec::with_capacity(RANDOM_MAX as usize);
    let db_pool: &DbPoolConnection = get_db_connection();
    let sql: String = format!(
        "SELECT id, randomNumber FROM {} LIMIT {}",
        TABLE_NAME_WORLD, RANDOM_MAX
    );
    if let Ok(rows) = db_query(&sql).fetch_all(db_pool).await {
        for row in rows {
            let id: i32 = row.get(KEY_ID);
            let random_number: i32 = row.get(KEY_RANDOM_NUMBER);
            res.push(QueryRow::new(id, random_number));
        }
    }
    res
}

pub async fn connection_db() -> DbPoolConnection {
    let db_url: &str = match option_env!("POSTGRES_URL") {
        Some(it) => it,
        _ => &format!(
            "{}://{}:{}@{}:{}/{}",
            DATABASE_TYPE,
            DATABASE_USER_NAME,
            DATABASE_USER_PASSWORD,
            DATABASE_HOST,
            DATABASE_PORT,
            DATABASE_NAME
        ),
    };
    let pool_size: u32 = (get_thread_count() as u32).min(DB_MAX_CONNECTIONS);
    let pool: DbPoolConnection = PgPoolOptions::new()
        .max_connections(DB_MAX_CONNECTIONS)
        .min_connections(pool_size)
        .max_lifetime(None)
        .test_before_acquire(false)
        .idle_timeout(None)
        .connect(db_url)
        .await
        .unwrap();
    pool
}

pub async fn get_update_data(limit: Queries) -> (Vec<QueryRow>, Vec<i32>, Vec<i32>) {
    let db_pool: &DbPoolConnection = get_db_connection();
    let mut query_res_list: Vec<QueryRow> = Vec::with_capacity(limit as usize);
    let rows: Vec<QueryRow> = get_some_row_id(limit, db_pool).await;
    let mut id_list: Vec<i32> = Vec::with_capacity(rows.len());
    let mut random_numbers: Vec<i32> = Vec::with_capacity(rows.len());
    for row in rows.iter() {
        let new_random_number: i32 = get_random_id() as i32;
        id_list.push(row.id);
        random_numbers.push(new_random_number);
        query_res_list.push(QueryRow::new(row.id, new_random_number));
    }
    (query_res_list, id_list, random_numbers)
}

pub async fn init_db() {
    #[cfg(feature = "dev")]
    {
        create_database().await;
        create_table().await;
        insert_records().await;
    }
    let _ = get_db_connection();
    let _ = CACHE.get(0);
}

pub async fn random_world_row(db_pool: &DbPoolConnection) -> QueryRow {
    let random_id: Queries = get_random_id();
    query_world_row(db_pool, random_id).await
}

pub async fn query_world_row(db_pool: &DbPoolConnection, id: Queries) -> QueryRow {
    let sql: &str = "SELECT id, randomNumber FROM World WHERE id = $1";
    if let Ok(rows) = db_query(sql).bind(id).fetch_one(db_pool).await {
        let random_number: i32 = rows.get(KEY_RANDOM_NUMBER);
        return QueryRow::new(id as i32, random_number);
    }
    return QueryRow::new(id as i32, 1);
}

pub async fn update_world_rows(limit: Queries) -> Vec<QueryRow> {
    let db_pool: &DbPoolConnection = get_db_connection();
    let (data, id_list, random_numbers) = get_update_data(limit).await;
    let sql: &str = "UPDATE World SET randomNumber = $1 WHERE id = $2";
    let mut tasks: Vec<JoinHandle<_>> = Vec::with_capacity(limit as usize);
    for (id, random_number) in id_list.into_iter().zip(random_numbers.into_iter()) {
        let db_pool: Pool<Postgres> = db_pool.clone();
        tasks.push(spawn(async move {
            db_query(sql)
                .bind(random_number)
                .bind(id)
                .execute(&db_pool)
                .await
        }));
    }
    join_all(tasks).await;
    data
}

pub async fn all_world_row() -> Vec<PgRow> {
    let db_pool: &DbPoolConnection = get_db_connection();
    let sql: String = format!("SELECT id, message FROM {}", TABLE_NAME_FORTUNE);
    let res: Vec<PgRow> = db_query(&sql).fetch_all(db_pool).await.unwrap_or_default();
    return res;
}

pub async fn get_some_row_id(limit: Queries, db_pool: &DbPoolConnection) -> Vec<QueryRow> {
    let tasks: Vec<_> = (0..limit)
        .map(|_| {
            let db_pool: Pool<Postgres> = db_pool.clone();
            spawn(async move {
                let id: i32 = get_random_id();
                query_world_row(&db_pool, id).await
            })
        })
        .collect();
    join_all(tasks)
        .await
        .into_iter()
        .filter_map(Result::ok)
        .collect()
}
