connectionString =
  "postgres://benchmarkdbuser:benchmarkdbpass@%s/hello_world?sslmode=disable"
worldSelect = "SELECT id, randomNumber FROM World WHERE id = $1"
worldUpdate = "UPDATE World SET randomNumber = $1 WHERE id = $2"
fortuneSelect = "SELECT id, message FROM Fortune"
worldRowCount = 10_000
maxConnections = 256

dbHost = "tfb-database"
dbPort = 5432
dbUser = "benchmarkdbuser"
dbPaswd = "benchmarkdbpass"
dbName = "hello_world"

worldSelectSQL = "SELECT id, randomNumber FROM World WHERE id = $1"
worldSelectCacheSQL = "SELECT id, randomNumber FROM World LIMIT $1"
worldUpdateSQL = "UPDATE World SET randomNumber = $1 WHERE id = $2"
fortuneSelectSQL = "SELECT id, message FROM Fortune"
