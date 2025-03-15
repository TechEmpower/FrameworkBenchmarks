use crate::*;

#[inline]
pub async fn get_db_connection() -> DbPoolConnection {
    let db_pool: DbPoolConnection = DB.read().await.clone().unwrap();
    db_pool
}

#[inline]
#[cfg(feature = "dev")]
pub async fn create_batabase() {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let _ = sqlx::query(&format!("CREATE DATABASE {};", DATABASE_NAME))
        .execute(&db_pool)
        .await;
}

#[inline]
#[cfg(feature = "dev")]
pub async fn create_table() {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let _ = sqlx::query(&format!(
        "CREATE TABLE IF NOT EXISTS {} (
            id SERIAL PRIMARY KEY, randomNumber INT NOT NULL
        );",
        TABLE_NAME_WORLD
    ))
    .execute(&db_pool)
    .await;
    let _ = sqlx::query(&format!(
        "CREATE TABLE IF NOT EXISTS {} (
            id SERIAL PRIMARY KEY, message VARCHAR NOT NULL
        );",
        TABLE_NAME_FORTUNE
    ))
    .execute(&db_pool)
    .await;
}

#[inline]
#[cfg(feature = "dev")]
pub async fn insert_records() {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let row: PgRow = sqlx::query(&format!("SELECT COUNT(*) FROM {}", TABLE_NAME_WORLD))
        .fetch_one(&db_pool)
        .await
        .unwrap();
    let count: i64 = row.get(0);
    let limit: i64 = ROW_LIMIT as i64;
    if count >= limit {
        return;
    }
    let missing_count: i64 = limit - count;
    let mut rng: rand::prelude::ThreadRng = rand::rng();
    let mut values: Vec<String> = Vec::new();
    for _ in 0..missing_count {
        let random_number: i32 = rng.random_range(1..=RANDOM_MAX);
        values.push(format!("(DEFAULT, {})", random_number));
    }
    let query: String = format!(
        "INSERT INTO {} (id, randomNumber) VALUES {}",
        TABLE_NAME_WORLD,
        values.join(",")
    );
    let _ = sqlx::query(&query).execute(&db_pool).await;
    let mut values: Vec<String> = Vec::new();
    for _ in 0..missing_count {
        let random_number: String = rng.random_range(1..=RANDOM_MAX).to_string();
        values.push(format!("(DEFAULT, {})", random_number));
    }
    let query: String = format!(
        "INSERT INTO {} (id, message) VALUES {}",
        TABLE_NAME_FORTUNE,
        values.join(",")
    );
    let _ = sqlx::query(&query).execute(&db_pool).await;
}

#[inline]
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
    let pool: DbPoolConnection = PgPoolOptions::new()
        .max_connections(1_000)
        .connect(db_url)
        .await
        .unwrap();
    pool
}

#[inline]
pub async fn init_query_state() {
    let mut query_sql: RwLockWriteGuard<'_, HashMap<Queries, String>> = QUERY_SQL.write().await;
    for random_id in 1..=ROW_LIMIT {
        query_sql.insert(
            random_id,
            format!(
                "SELECT id, randomNumber FROM {} WHERE id = {}",
                TABLE_NAME_WORLD, random_id
            ),
        );
    }
}

#[inline]
pub async fn get_update_data(limit: Queries) -> (String, Vec<QueryRow>) {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let mut query_res_list: Vec<QueryRow> = Vec::with_capacity(limit as usize);
    let rows: Vec<PgRow> = get_some_row_id(limit, &db_pool).await.unwrap_or_default();
    let mut query: String = format!("UPDATE {} SET randomNumber = CASE id ", TABLE_NAME_WORLD);
    let mut id_list: Vec<i32> = Vec::with_capacity(limit as usize);
    let mut value_list: String = String::new();
    let mut id_in_clause: String = String::new();
    for (i, row) in rows.iter().enumerate() {
        let new_random_number: i32 = rand::rng().random_range(1..RANDOM_MAX);
        let id: i32 = row.get(KEY_ID);
        id_list.push(id);
        value_list.push_str(&format!("WHEN {} THEN {} ", id, new_random_number));
        if i > 0 {
            id_in_clause.push_str(", ");
        }
        id_in_clause.push_str(&id.to_string());
        query_res_list.push(QueryRow::new(id, new_random_number));
    }
    query.push_str(&value_list);
    query.push_str(&format!("END WHERE id IN ({})", id_in_clause));
    (query, query_res_list)
}

#[inline]
pub async fn init_db() {
    {
        let mut db_pool_lock: RwLockWriteGuard<'_, Option<DbPoolConnection>> = DB.write().await;
        *db_pool_lock = Some(connection_db().await);
    }
    #[cfg(feature = "dev")]
    {
        create_batabase().await;
        create_table().await;
        insert_records().await;
    }
    init_query_state().await;
}

#[inline]
pub async fn random_world_row(db_pool: &DbPoolConnection) -> Result<QueryRow, Box<dyn Error>> {
    let random_id: i32 = rand::rng().random_range(1..=ROW_LIMIT);
    let sql: String = QUERY_SQL.read().await.get(&random_id).cloned().unwrap();
    if let Ok(rows) = sqlx::query(&sql).fetch_one(db_pool).await {
        let id: i32 = rows.get(KEY_ID);
        let random_number: i32 = rows.get(KEY_RANDOM_NUMBER);
        return Ok(QueryRow::new(id, random_number));
    }
    return Ok(QueryRow::new(1, 1));
}

#[inline]
pub async fn update_world_rows(limit: Queries) -> Result<Vec<QueryRow>, Box<dyn Error>> {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let (sql, data) = get_update_data(limit).await;
    spawn(async move {
        let _ = sqlx::query(&sql).execute(&db_pool).await;
    });
    Ok(data)
}

#[inline]
pub async fn all_world_row() -> Vec<PgRow> {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let query_all_sql: String = format!("SELECT id, randomNumber FROM {}", TABLE_NAME_FORTUNE);
    let sql: String = query_all_sql.clone();
    let res: Vec<PgRow> = sqlx::query(&sql)
        .fetch_all(&db_pool)
        .await
        .unwrap_or_default();
    return res;
}

#[inline]
pub async fn get_some_row_id(
    limit: Queries,
    db_pool: &DbPoolConnection,
) -> Result<Vec<PgRow>, SqlxError> {
    let sql: String = format!("SELECT id FROM {} LIMIT {}", TABLE_NAME_WORLD, limit);
    let res: Result<Vec<PgRow>, SqlxError> = sqlx::query(&sql).fetch_all(db_pool).await;
    return res;
}
