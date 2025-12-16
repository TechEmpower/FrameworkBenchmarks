package database

import com.zaxxer.hikari.HikariConfig

fun HikariConfig.configurePostgres(poolSize: Int) {
    jdbcUrl = "jdbc:postgresql://tfb-database/hello_world?useSSL=false"
    driverClassName = org.postgresql.Driver::class.java.name

    configureCommon(poolSize)
}

fun HikariConfig.configureCommon(poolSize: Int) {
    username = "benchmarkdbuser"
    password = "benchmarkdbpass"
    addDataSourceProperty("cacheServerConfiguration", true)
    addDataSourceProperty("cachePrepStmts", "true")
    addDataSourceProperty("useUnbufferedInput", "false")
    addDataSourceProperty("prepStmtCacheSize", "4096")
    addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
    connectionTimeout = 10000
    maximumPoolSize = poolSize
    minimumIdle = poolSize
}
