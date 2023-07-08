package com.hexagonkt.async.store

import com.hexagonkt.Settings
import com.hexagonkt.core.Jvm
import com.hexagonkt.model.CachedWorld
import com.hexagonkt.model.Fortune
import com.hexagonkt.model.World
import io.vertx.core.CompositeFuture.all
import io.vertx.core.Future
import io.vertx.core.Vertx.vertx
import io.vertx.pgclient.PgConnectOptions
import io.vertx.pgclient.PgPool
import io.vertx.sqlclient.*
import org.cache2k.Cache
import java.util.concurrent.CompletableFuture

class BenchmarkPgClientStore(
    engine: String,
    private val settings: Settings = Settings(),
) : BenchmarkStore(settings) {

    companion object {
        private const val SELECT_WORLD: String = "select * from world where id = $1"
        private const val UPDATE_WORLD: String = "update world set randomNumber = $1 where id = $2"
        private const val SELECT_ALL_FORTUNES: String = "select * from fortune"
    }

    private val connectOptions: PgConnectOptions by lazy {
        PgConnectOptions().apply {
            host = Jvm.systemSettingOrNull("${engine.uppercase()}_DB_HOST") ?: "tfb-database"
            database = settings.databaseName
            user = settings.databaseUsername
            password = settings.databasePassword
            cachePreparedStatements = true
        }
    }

    private val poolOptions: PoolOptions by lazy {
        PoolOptions().apply {
            val environment = Jvm.systemSettingOrNull<String>("BENCHMARK_ENV")?.lowercase()
            maxSize = 8 + if (environment == "citrine") Jvm.cpuCount else Jvm.cpuCount * 2
        }
    }

    private val dataSource: SqlClient by lazy { PgPool.pool(vertx(), connectOptions, poolOptions) }

    override fun findAllFortunes(): CompletableFuture<List<Fortune>> =
        dataSource.preparedQuery(SELECT_ALL_FORTUNES)
            .execute()
            .map { rowSet ->
                rowSet.map { row -> Fortune(row.getInteger(0), row.getString(1)) }
            }
            .toCompletionStage()
            .toCompletableFuture()

    override fun findWorlds(ids: List<Int>): CompletableFuture<List<World>> {
        val futures = ids.map { findWorld(it, dataSource) }
        return all(futures).map { it.list<World>() }.toCompletionStage().toCompletableFuture()
    }

    override fun replaceWorlds(worlds: List<World>): CompletableFuture<*> {
        val futures = worlds.map {
            val worldId = it.id
            val newRandomNumber = it.randomNumber
            dataSource
                .preparedQuery(SELECT_WORLD)
                .execute(Tuple.of(worldId))
                .flatMap { rowSet ->
                    val row = rowSet.iterator().next()
                    row.getInteger(1) // Read 'randomNumber' to comply with Test type 5, point 6
                    dataSource
                        .preparedQuery(UPDATE_WORLD)
                        .execute(Tuple.of(newRandomNumber, worldId))
                }
        }
        return all(futures).toCompletionStage().toCompletableFuture()
    }

    override fun initWorldsCache(cache: Cache<Int, CachedWorld>) {
        dataSource
            .preparedQuery("select * from world")
            .execute()
            .map { rowSet ->
                rowSet.map { row ->
                    val id = row.getInteger(0)
                    val randomNumber = row.getInteger(1)
                    cache.put(id, CachedWorld(id, randomNumber))
                }
            }
            .toCompletionStage()
            .toCompletableFuture()
            .get()
    }

    override fun loadCachedWorld(id: Int): CachedWorld =
        findWorld(id, dataSource)
            .map { world -> CachedWorld(world.id, world.randomNumber) }
            .toCompletionStage()
            .toCompletableFuture()
            .get()

    override fun close() {
        dataSource.close()
    }

    private fun findWorld(id: Int, client: SqlClient): Future<World> =
        client
            .preparedQuery(SELECT_WORLD)
            .execute(Tuple.of(id))
            .map { rowSet ->
                val row = rowSet.iterator().next()
                World(row.getInteger(0), row.getInteger(1))
            }
}
