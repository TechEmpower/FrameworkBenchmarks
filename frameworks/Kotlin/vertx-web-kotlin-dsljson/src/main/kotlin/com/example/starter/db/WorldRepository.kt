package com.example.starter.db

import com.example.starter.models.World
import io.vertx.core.Future
import io.vertx.core.Promise
import io.vertx.core.impl.future.CompositeFutureImpl
import io.vertx.pgclient.PgConnection
import io.vertx.sqlclient.PreparedQuery
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.RowSet
import io.vertx.sqlclient.Tuple
import io.vertx.sqlclient.impl.SqlClientInternal
import io.vertx.sqlclient.internal.ArrayTuple
import java.util.concurrent.ThreadLocalRandom

@Suppress("NOTHING_TO_INLINE", "UNCHECKED_CAST")
class WorldRepository private constructor(
    conn: PgConnection,
    private val selectWorldQuery: PreparedQuery<RowSet<Row>>,
    private val updateWorldQueries: Array<PreparedQuery<RowSet<Row>>>,
) : AbstractRepository<World>(conn) {

    fun selectRandomWorld(): Future<World> = selectWorldQuery
        .execute(Tuple.of(randomWorld()))
        .map { map(it.first()) }

    fun selectRandomWorlds(numWorlds: Int): Future<Array<World>> {
        val promise = Promise.promise<Array<World>>()
        val arr = arrayOfNulls<World>(numWorlds)
        var count = 0
        (this.conn as SqlClientInternal).group { c ->
            val query = c.preparedQuery(SELECT_WORLD_SQL)
            repeat(numWorlds) { _ ->
                query.execute(Tuple.of(randomWorld()))
                    .onComplete { ar ->
                        when {
                            ar.succeeded() -> {
                                val result = ar.result()
                                val index = count++
                                arr[index] = map(result.iterator().next())
                                if (index == numWorlds - 1) {
                                    promise.complete(arr as Array<World>)
                                }
                            }
                            else -> promise.tryFail(ar.cause())
                        }
                    }
            }
        }
        return promise.future()
    }

    fun updateRandomWorlds(numWorlds: Int): Future<Array<World>> = selectRandomWorlds(numWorlds)
        .flatMap { worlds ->
            val params = ArrayTuple(worlds.size * 3)
            worlds.forEach {
                it.randomNumber = randomWorld()
                params.addValue(it.id)
                params.addValue(it.randomNumber)
            }
            worlds.forEach {
                params.addValue(it.id)
            }
            updateWorldQueries[numWorlds - 1].execute(params).map { worlds }
        }

    companion object {
        private const val SELECT_WORLD_SQL = "SELECT id, randomnumber FROM world WHERE id = $1"

        private inline fun randomWorld(): Int = 1 + ThreadLocalRandom.current().nextInt(10_000)

        private inline fun map(row: Row): World = World(
            row.getInteger(0),
            row.getInteger(1),
        )

        fun init(conn: PgConnection): Future<WorldRepository> = conn.let { conn ->
            val selectWorldQuery = conn.prepare(SELECT_WORLD_SQL).map { ps -> ps.query() }
            val updateWorldQueries = run {
                val queries = arrayOfNulls<PreparedQuery<RowSet<Row>>>(500)
                Array(500) { num ->
                    val count = num + 1
                    var paramIndex = 1
                    val sb = StringBuilder()
                    sb.append("UPDATE world SET randomnumber = CASE id ")
                    repeat(count) {
                        sb.append("WHEN $${paramIndex++} THEN $${paramIndex++} ")
                    }
                    sb.append("ELSE randomnumber END WHERE id IN (")
                    repeat(count) {
                        sb.append("$${paramIndex++},")
                    }
                    sb[sb.length - 1] = ')'

                    conn
                        .prepare(sb.toString())
                        .map { ps ->
                            queries[num] = ps.query()
                        }
                }.let {
                    CompositeFutureImpl.all(*it).map {
                        queries as Array<PreparedQuery<RowSet<Row>>>
                    }
                }
            }

            CompositeFutureImpl.all(selectWorldQuery, updateWorldQueries).map {
                WorldRepository(
                    conn,
                    selectWorldQuery.result(),
                    updateWorldQueries.result(),
                )
            }
        }
    }
}
