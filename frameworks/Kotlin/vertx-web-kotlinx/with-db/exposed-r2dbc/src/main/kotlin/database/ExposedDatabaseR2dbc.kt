package database

import org.jetbrains.exposed.v1.core.vendors.PostgreSQLDialect
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabase
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabaseConfig

// Configuration variants for benchmarking
fun r2dbcDatabaseConnectPoolOriginal(connectionPoolSize: Int) =
    R2dbcDatabase.connect(connectionPoolOriginal(connectionPoolSize), R2dbcDatabaseConfig {
        explicitDialect = PostgreSQLDialect()
    })

fun r2dbcDatabaseConnectPoolOptimized(connectionPoolSize: Int) =
    R2dbcDatabase.connect(connectionPoolOptimized(connectionPoolSize), R2dbcDatabaseConfig {
        explicitDialect = PostgreSQLDialect()
    })

/**
 * Creates a shared R2dbcDatabase instance with a connection pool.
 * Used for shared-pool benchmark configurations.
 */
fun r2dbcConnectPool(poolSize: Int, useOptimizedConfig: Boolean): R2dbcDatabase =
    if (useOptimizedConfig)
        r2dbcDatabaseConnectPoolOptimized(poolSize)
    else
        r2dbcDatabaseConnectPoolOriginal(poolSize)
