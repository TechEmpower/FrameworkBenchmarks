package database

import io.r2dbc.pool.ConnectionPool
import io.r2dbc.pool.ConnectionPoolConfiguration
import io.r2dbc.spi.ConnectionFactories
import io.r2dbc.spi.ConnectionFactoryOptions
import io.r2dbc.spi.ConnectionFactoryOptions.DRIVER

// Original configuration before optimizations
val connectionFactoryOriginal = ConnectionFactories.get(
    ConnectionFactoryOptions.builder()
        .option(DRIVER, "postgresql")
        .option(ConnectionFactoryOptions.HOST, HOST)
        .option(ConnectionFactoryOptions.USER, USER)
        .option(ConnectionFactoryOptions.PASSWORD, PASSWORD)
        .option(ConnectionFactoryOptions.DATABASE, DATABASE)
        .build()
)

fun connectionPoolConfigurationOriginal(size: Int) =
    ConnectionPoolConfiguration.builder(connectionFactoryOriginal)
        .initialSize(size)
        .maxSize(size)
        .build()

fun connectionPoolOriginal(size: Int) =
    ConnectionPool(connectionPoolConfigurationOriginal(size))
