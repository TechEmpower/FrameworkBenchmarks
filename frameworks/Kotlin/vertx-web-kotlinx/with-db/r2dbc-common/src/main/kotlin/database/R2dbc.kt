package database

import io.r2dbc.spi.ConnectionFactories.get
import io.r2dbc.spi.ConnectionFactoryOptions
import io.r2dbc.spi.ConnectionFactoryOptions.DRIVER

// not used currently
// Note that this URL doesn't have `USER` and `PASSWORD`
const val POSTGRESQL_R2DBC_URL = "r2dbc:postgresql://$HOST:5432/$DATABASE"

val connectionFactory = get(
    ConnectionFactoryOptions.builder()
        .option(DRIVER, "postgresql")
        .option(ConnectionFactoryOptions.HOST, HOST)
        .option(ConnectionFactoryOptions.USER, USER)
        .option(ConnectionFactoryOptions.PASSWORD, PASSWORD)
        .option(ConnectionFactoryOptions.DATABASE, DATABASE)
        .build()
)
