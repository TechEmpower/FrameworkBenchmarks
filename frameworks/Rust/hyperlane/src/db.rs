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
    let connection: DbConnection = db_pool.get().await.unwrap();
    let db_exists: bool = connection
        .query_one(
            "SELECT EXISTS(SELECT 1 FROM pg_database WHERE datname = $1);",
            &[&DATABASE_NAME],
        )
        .await
        .unwrap()
        .get(0);
    if !db_exists {
        connection
            .batch_execute(&format!("CREATE DATABASE {};", DATABASE_NAME))
            .await
            .unwrap();
    }
}

#[inline]
#[cfg(feature = "dev")]
pub async fn create_table() {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let connection: DbConnection = db_pool.get().await.unwrap();
    connection
        .batch_execute(&format!(
            "CREATE TABLE IF NOT EXISTS {} (
                    id SERIAL PRIMARY KEY,
                    randomNumber INTEGER NOT NULL
                );",
            TABLE_NAME
        ))
        .await
        .unwrap();
}

#[inline]
#[cfg(feature = "dev")]
pub async fn insert_records() {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let connection: DbConnection = db_pool.get().await.unwrap();
    let row: Row = connection
        .query_one(&format!("SELECT COUNT(*) FROM {}", TABLE_NAME), &[])
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
        TABLE_NAME,
        values.join(",")
    );
    connection.batch_execute(&query).await.unwrap();
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
    let config: Config = db_url.parse::<Config>().unwrap();
    let db_manager: PostgresConnectionManager<NoTls> =
        PostgresConnectionManager::new(config, NoTls);
    let db_pool: DbPoolConnection = Pool::builder()
        .max_size(DB_POOL_SIZE)
        .max_lifetime(Some(Duration::from_secs(u64::MAX)))
        .connection_timeout(Duration::from_secs(u64::MAX))
        .build(db_manager)
        .await
        .unwrap();
    db_pool
}

#[inline]
pub async fn init_query_state() {
    let mut query_sql: RwLockWriteGuard<'_, HashMap<Queries, String>> = QUERY_SQL.write().await;
    for random_id in 1..=ROW_LIMIT {
        query_sql.insert(
            random_id,
            format!(
                "SELECT id, randomNumber FROM {} WHERE id = {}",
                TABLE_NAME, random_id
            ),
        );
    }
}

#[inline]
pub async fn init_query_all_state() {
    let mut query_all_sql: RwLockWriteGuard<'_, String> = QUERY_ALL_SQL.write().await;
    *query_all_sql = format!("SELECT id, randomNumber FROM {}", TABLE_NAME);
}

#[inline]
pub async fn init_update_state() {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let mut update_sql: RwLockWriteGuard<'_, HashMap<Queries, String>> = UPDATE_SQL.write().await;
    let mut update_query_sql: RwLockWriteGuard<'_, HashMap<Queries, Vec<QueryRow>>> =
        UPDATE_QUERY_SQL.write().await;
    for limit in 1..=ROW_LIMIT {
        let limit: Queries = limit as Queries;
        let mut query_res_list: Vec<QueryRow> = Vec::with_capacity(limit as usize);
        let rows: Vec<Row> = get_some_row_id(limit, &db_pool).await.unwrap_or_default();
        let mut query = format!("UPDATE {} SET randomNumber = CASE id ", TABLE_NAME);
        let mut id_list: Vec<i32> = Vec::with_capacity(limit as usize);
        let mut value_list = String::new();
        let mut id_in_clause = String::new();
        for (i, row) in rows.iter().enumerate() {
            let new_random_number: i32 = rand::rng().random_range(1..RANDOM_MAX);
            let id: i32 = row.get(0);
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
        update_query_sql.insert(limit, query_res_list);
        update_sql.insert(limit, query);
    }
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
    init_query_all_state().await;
    init_update_state().await;
}

#[inline]
pub async fn random_world_row(db_pool: &DbPoolConnection) -> Result<QueryRow, Box<dyn Error>> {
    let random_id: i32 = rand::rng().random_range(1..=ROW_LIMIT);
    let connection: DbConnection = db_pool
        .get()
        .await
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("timeout: {}", e)))?;
    let sql: String = QUERY_SQL.read().await.get(&random_id).cloned().unwrap();
    if let Some(rows) = connection.query_opt(&sql, &[]).await? {
        let id: i32 = rows.get(0);
        let random_number: i32 = rows.get(1);
        return Ok(QueryRow::new(id, random_number));
    }
    return Ok(QueryRow::new(1, 1));
}

#[inline]
pub async fn update_world_rows(limit: Queries) -> Result<Vec<QueryRow>, Box<dyn Error>> {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let connection: DbConnection = db_pool
        .get()
        .await
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("timeout: {}", e)))?;
    let sql: String = UPDATE_SQL.read().await.get(&limit).unwrap().clone();
    let _ = connection.execute(&sql, &[]).await;
    let list: Vec<QueryRow> = UPDATE_QUERY_SQL.read().await.get(&limit).cloned().unwrap();
    Ok(list)
}

#[inline]
pub async fn all_world_row() -> Result<Vec<Row>, Box<dyn Error>> {
    let db_pool: DbPoolConnection = get_db_connection().await;
    let connection: DbConnection = db_pool
        .get()
        .await
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("timeout: {}", e)))?;
    let query_all_sql: RwLockReadGuard<'_, String> = QUERY_ALL_SQL.read().await;
    let sql: String = query_all_sql.clone();
    let rows: Vec<Row> = connection.query(&sql, &[]).await?;
    return Ok(rows);
}

#[inline]
pub async fn get_some_row_id(
    limit: Queries,
    db_pool: &DbPoolConnection,
) -> Result<Vec<Row>, Box<dyn Error>> {
    let connection: DbConnection = db_pool
        .get()
        .await
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("timeout: {}", e)))?;
    let sql: Statement = connection
        .prepare(&format!("SELECT id FROM {} LIMIT {}", TABLE_NAME, limit))
        .await?;
    let rows: Vec<Row> = connection.query(&sql, &[]).await?;
    return Ok(rows);
}
