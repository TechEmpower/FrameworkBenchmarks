package database

import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabase

fun r2DbcDatabaseConnect() =
    R2dbcDatabase.connect(POSTGRESQL_R2DBC_URL, user = USER, password = PASSWORD)
