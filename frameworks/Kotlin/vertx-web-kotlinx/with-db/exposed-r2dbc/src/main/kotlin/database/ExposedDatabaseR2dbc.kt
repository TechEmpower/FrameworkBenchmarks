package database

import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabase
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabaseConfig

fun r2DbcDatabaseConnect() =
    R2dbcDatabase.connect(connectionFactory, R2dbcDatabaseConfig())
