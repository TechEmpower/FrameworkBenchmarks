use crate::*;

pub async fn get_db_connection() -> DbPoolConnection {
    let db_pool: DbPoolConnection = DB.read().await.clone().unwrap();
    db_pool
}

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
        println_warning!(
            "database `",
            DATABASE_NAME,
            "` not found. Creating database..."
        );
        connection
            .batch_execute(&format!("CREATE DATABASE {};", DATABASE_NAME))
            .await
            .unwrap();
        println_success!("database `", DATABASE_NAME, "` created successfully");
    }
    println_success!("database `", DATABASE_NAME, "` ready");
}

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
    println_success!("table `", TABLE_NAME, "` ready");
}

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
        println_warning!(format!(
            "table '{}' already has {} records. No need to insert.",
            TABLE_NAME, count
        ));
        return;
    }
    let missing_count: i64 = limit - count;
    println_warning!(format!(
        "table '{}' has {} records. Inserting {} missing records...",
        TABLE_NAME, count, missing_count
    ));
    let mut rng: rand::prelude::ThreadRng = rand::rng();
    let mut values: Vec<String> = Vec::new();
    for _ in 0..missing_count {
        let random_number: i32 = rng.random_range(1..=10000);
        values.push(format!("(DEFAULT, {})", random_number));
    }
    let query: String = format!(
        "INSERT INTO {} (id, randomNumber) VALUES {}",
        TABLE_NAME,
        values.join(",")
    );
    connection.batch_execute(&query).await.unwrap();
    println_success!(format!(
        "successfully inserted {} missing records into '{}' table.",
        TABLE_NAME, missing_count
    ));
}

pub async fn init_db() {
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
    println_warning!("db url: ", db_url);
    let config: Config = db_url.parse::<Config>().unwrap();
    let db_manager: PostgresConnectionManager<NoTls> =
        PostgresConnectionManager::new(config, NoTls);
    let db_pool: DbPoolConnection = Pool::builder().build(db_manager).await.unwrap();
    {
        let mut db_pool_lock: RwLockWriteGuard<'_, Option<DbPoolConnection>> = DB.write().await;
        *db_pool_lock = Some(db_pool.clone());
    }
    create_batabase().await;
    create_table().await;
    insert_records().await;
}

pub async fn random_world_row() -> Result<QueryRow, Box<dyn std::error::Error>> {
    let random_id: i32 = rand::rng().random_range(1..ROW_LIMIT);
    let db_pool: DbPoolConnection = get_db_connection().await;
    let connection: DbConnection = db_pool
        .get()
        .await
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("timeout: {}", e)))?;
    let stmt: Statement = connection
        .prepare(&format!(
            "SELECT id, randomNumber FROM {} WHERE id = $1",
            TABLE_NAME
        ))
        .await?;
    if let Some(rows) = connection.query_opt(&stmt, &[&random_id]).await? {
        let id: i32 = rows.get(0);
        let random_number: i32 = rows.get(1);
        return Ok((id, random_number));
    }
    return Ok((0, 0));
}
