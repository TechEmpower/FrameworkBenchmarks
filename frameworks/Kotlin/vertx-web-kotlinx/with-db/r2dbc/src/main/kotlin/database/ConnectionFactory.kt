package database

import io.r2dbc.spi.ConnectionFactories.get
import io.r2dbc.spi.ConnectionFactoryOptions
import io.r2dbc.spi.ConnectionFactoryOptions.DRIVER

val connectionFactory = get(
    ConnectionFactoryOptions.builder()
        .option(DRIVER, "postgresql")
        .option(ConnectionFactoryOptions.HOST, HOST)
        .option(ConnectionFactoryOptions.USER, USER)
        .option(ConnectionFactoryOptions.PASSWORD, PASSWORD)
        .option(ConnectionFactoryOptions.DATABASE, DATABASE)
        .build()
)
