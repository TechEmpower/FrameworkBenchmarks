package org.jetbrains.ktor.benchmarks

import com.zaxxer.hikari.HikariConfig

fun HikariConfig.configurePostgres(poolSize: Int) {
    jdbcUrl = "jdbc:postgresql://tfb-database:5432/hello_world?loggerLevel=OFF&disableColumnSanitiser=true&assumeMinServerVersion=16&sslmode=disable"
    driverClassName = org.postgresql.Driver::class.java.name
    configureCommon(poolSize)
}

fun HikariConfig.configureCommon(poolSize: Int) {
    username = "benchmarkdbuser"
    password = "benchmarkdbpass"
    addDataSourceProperty("cacheServerConfiguration", true)
    connectionTimeout = 5000
    maximumPoolSize = poolSize
    minimumIdle = poolSize
    idleTimeout = 300000 // 5 minutes
    maxLifetime = 600000 // 10 minutes
    validationTimeout = 5000
    leakDetectionThreshold = 60000
}

fun HikariConfig.configureMySql(poolSize: Int) {
    jdbcUrl = "jdbc:mysql://tfb-database:3306/hello_world?useSSL=false"
    driverClassName = com.mysql.jdbc.Driver::class.java.name
    configureCommon(poolSize)
}
