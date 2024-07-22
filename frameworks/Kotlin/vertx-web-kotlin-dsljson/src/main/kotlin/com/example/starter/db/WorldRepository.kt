package com.example.starter.db

import com.example.starter.models.World
import com.example.starter.utils.array
import io.vertx.core.Future
import io.vertx.pgclient.PgConnection
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.Tuple
import java.util.concurrent.ThreadLocalRandom

@Suppress("NOTHING_TO_INLINE")
class WorldRepository(conn: PgConnection) : AbstractRepository<World>(conn) {
    private val selectWorldQuery = this.conn.preparedQuery(SELECT_WORLD_SQL)
    private val updateWordQuery = this.conn.preparedQuery(UPDATE_WORLD_SQL)

    fun selectRandomWorld(): Future<World> = selectWorldQuery
        .execute(Tuple.of(randomWorld()))
        .map { map(it.single()) }

    fun selectRandomWorlds(numWorlds: Int): Future<Array<World>> = Future
        .all(List(numWorlds) { selectRandomWorld() })
        .map { it.array() }

    fun updateRandomWorlds(numWorlds: Int): Future<Array<World>> = selectRandomWorlds(numWorlds)
        .flatMap { worlds ->
            worlds.sort()
            worlds.forEach { it.randomNumber = randomWorld() }
            updateWordQuery
                .executeBatch(worlds.map { Tuple.of(it.randomNumber, it.id) })
                .map { _ -> worlds }
        }

    companion object {
        private const val SELECT_WORLD_SQL = "SELECT id, randomNumber from WORLD where id = $1"
        private const val UPDATE_WORLD_SQL = "UPDATE world SET randomNumber = $1 WHERE id = $2"

        private inline fun randomWorld(): Int = 1 + ThreadLocalRandom.current().nextInt(10000)
        private inline fun map(row: Row): World = World(row.getInteger(0), row.getInteger(1))
    }
}
