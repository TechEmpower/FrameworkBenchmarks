package com.example.starter.db

import com.example.starter.models.World
import io.vertx.core.Future
import io.vertx.core.Promise
import io.vertx.pgclient.PgConnection
import io.vertx.sqlclient.PreparedQuery
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.RowSet
import io.vertx.sqlclient.Tuple
import io.vertx.sqlclient.impl.ArrayTuple
import io.vertx.sqlclient.impl.SqlClientInternal
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicInteger

@Suppress("NOTHING_TO_INLINE", "UNCHECKED_CAST")
class WorldRepository(conn: PgConnection) : AbstractRepository<World>(conn) {
    private val selectWorldQuery = this.conn.preparedQuery(SELECT_WORLD_SQL)
    private val updateWorldQueries = generateQueries(this.conn)

    fun selectRandomWorld(): Future<World> = selectWorldQuery
        .execute(Tuple.of(randomWorld()))
        .map { map(it.iterator().next()) }

    fun selectRandomWorlds(numWorlds: Int): Future<Array<World>> {
        val promise = Promise.promise<Array<World>>()
        val arr = arrayOfNulls<World>(numWorlds)
        val count = AtomicInteger(0)
        (this.conn as SqlClientInternal).group { c ->
            repeat(numWorlds) {
                c.preparedQuery(SELECT_WORLD_SQL).execute(Tuple.of(randomWorld())) { ar ->
                    val index = count.getAndIncrement()
                    arr[index] = map(ar.result().iterator().next())
                    if (index == numWorlds - 1) {
                        promise.complete(arr as Array<World>)
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

        private inline fun randomWorld(): Int = 1 + ThreadLocalRandom.current().nextInt(10000)
        private inline fun map(row: Row): World = World(row.getInteger(0), row.getInteger(1))

        private fun generateQueries(conn: PgConnection): Array<PreparedQuery<RowSet<Row>>> {
            val arr = arrayOfNulls<PreparedQuery<RowSet<Row>>>(500)
            for (num in 1..500) {
                var paramIndex = 1
                val sb = StringBuilder()
                sb.append("UPDATE world SET randomnumber = CASE id ")
                for (i in 1..num) {
                    sb.append("WHEN \$$paramIndex THEN \$${paramIndex + 1} ")
                    paramIndex += 2
                }
                sb.append("ELSE randomnumber END WHERE id IN (")
                for (i in 1..num) {
                    sb.append("\$$paramIndex,")
                    paramIndex += 1
                }
                sb[sb.length - 1] = ')'
                arr[num - 1] = conn.preparedQuery(sb.toString())
            }
            return arr as Array<PreparedQuery<RowSet<Row>>>
        }
    }
}
