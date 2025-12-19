package database

import io.r2dbc.spi.ConnectionFactories

val connectionFactory = ConnectionFactories.get(POSTGRESQL_R2DBC_URL)
