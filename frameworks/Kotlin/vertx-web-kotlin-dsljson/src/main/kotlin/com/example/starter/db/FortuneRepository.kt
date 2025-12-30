package com.example.starter.db

import com.example.starter.models.Fortune
import com.example.starter.utils.mapToArray

import io.vertx.core.Future
import io.vertx.pgclient.PgConnection
import io.vertx.sqlclient.PreparedQuery
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.RowSet

@Suppress("NOTHING_TO_INLINE")
class FortuneRepository(
    conn: PgConnection,
    private val selectFortunesQuery: PreparedQuery<RowSet<Row>>,
) : AbstractRepository<Fortune>(conn) {

    fun selectFortunes(): Future<Array<Fortune>> = selectFortunesQuery
        .execute()
        .map { it.mapToArray(FortuneRepository::map) }

    companion object {
        private const val SELECT_FORTUNES_SQL = "SELECT id, message FROM fortune"

        private inline fun map(row: Row): Fortune = Fortune(
            row.getInteger(0),
            row.getString(1),
        )

        fun init(conn: PgConnection): Future<FortuneRepository> = conn.let { conn ->
            conn.prepare(SELECT_FORTUNES_SQL).map { ps ->
                FortuneRepository(
                    conn,
                    ps.query(),
                )
            }
        }
    }
}