package database

import org.jetbrains.exposed.v1.core.vendors.PostgreSQLDialect
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabase
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabaseConfig

// Configuration variants for benchmarking
fun r2dbcDatabaseConnectPoolOriginal(connectionPoolSize: Int) =
    R2dbcDatabase.connect(connectionPoolOriginal(connectionPoolSize), R2dbcDatabaseConfig {
        explicitDialect = PostgreSQLDialect()
    })

fun r2dbcDatabaseConnectPoolKtorR2dbc(connectionPoolSize: Int) =
    R2dbcDatabase.connect(connectionPoolKtorR2dbc(connectionPoolSize), R2dbcDatabaseConfig {
        explicitDialect = PostgreSQLDialect()
    })

/**
 * Creates a shared R2dbcDatabase instance with a connection pool.
 * Used for shared-pool benchmark configurations.
 */
fun r2dbcConnectPool(poolSize: Int, useKtorR2dbcConfig: Boolean): R2dbcDatabase =
    if (useKtorR2dbcConfig)
        r2dbcDatabaseConnectPoolKtorR2dbc(poolSize)
    else
        r2dbcDatabaseConnectPoolOriginal(poolSize)
