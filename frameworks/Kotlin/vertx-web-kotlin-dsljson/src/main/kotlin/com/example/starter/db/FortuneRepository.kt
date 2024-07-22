package com.example.starter.db

import com.example.starter.models.Fortune
import io.vertx.core.Future
import io.vertx.pgclient.PgConnection
import io.vertx.sqlclient.Row

class FortuneRepository(conn: PgConnection) : AbstractRepository<Fortune>(conn) {
    private val selectFortuneQuery = this.conn.preparedQuery(SELECT_FORTUNE_SQL)

    fun selectFortunes(): Future<Array<Fortune>> = selectFortuneQuery
        .execute()
        .map { it.map { row -> map(row) }.toTypedArray() }

    companion object {
        private const val SELECT_FORTUNE_SQL = "SELECT id, message from FORTUNE"

        @Suppress("NOTHING_TO_INLINE")
        private inline fun map(row: Row): Fortune = Fortune(row.getInteger(0), row.getString(1))
    }
}