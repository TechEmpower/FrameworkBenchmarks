package com.hexagonkt.async.store

import com.github.jasync.sql.db.ConnectionPoolConfigurationBuilder
import com.github.jasync.sql.db.pool.ConnectionPool
import com.github.jasync.sql.db.postgresql.PostgreSQLConnection
import com.github.jasync.sql.db.postgresql.PostgreSQLConnectionBuilder
import com.hexagonkt.Settings
import com.hexagonkt.core.Jvm
import com.hexagonkt.model.CachedWorld
import com.hexagonkt.model.Fortune
import com.hexagonkt.model.World
import org.cache2k.Cache
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletableFuture.allOf

class BenchmarkJasyncStore(
    engine: String,
    private val settings: Settings = Settings(),
) : BenchmarkStore(settings) {

    companion object {
        private const val SELECT_WORLD: String = "select * from world where id = $1"
        private const val UPDATE_WORLD: String = "update world set randomNumber = $1 where id = $2"
        private const val SELECT_ALL_FORTUNES: String = "select * from fortune"
    }

    private val dataSource: ConnectionPool<PostgreSQLConnection> by lazy {
        ConnectionPoolConfigurationBuilder().let {
            val environment = Jvm.systemSettingOrNull<String>("BENCHMARK_ENV")?.lowercase()
            val cpuCount = Jvm.cpuCount

            it.maxActiveConnections = 8 + if (environment == "citrine") cpuCount else cpuCount * 2
            it.host = Jvm.systemSettingOrNull("${engine.uppercase()}_DB_HOST") ?: "tfb-database"
            it.database = settings.databaseName
            it.username = settings.databaseUsername
            it.password = settings.databasePassword

            PostgreSQLConnectionBuilder.createConnectionPool(it.build())
        }
    }

    override fun findAllFortunes(): CompletableFuture<List<Fortune>> =
        dataSource.sendPreparedStatement(SELECT_ALL_FORTUNES)
            .thenApply { result ->
                val rowSet = result.rows
                rowSet.map { row ->
                    val id = row.getInt(0) ?: error("")
                    val message = row.getString(1) ?: error("")
                    Fortune(id, message)
                }
            }

    override fun findWorlds(ids: List<Int>): CompletableFuture<List<World>> {
        val futures = ids.map { findWorld(it, dataSource) }.toTypedArray()
        return allOf(*futures).thenApply { futures.map { f -> f.get() ?: error("") } }
    }

    override fun replaceWorlds(worlds: List<World>): CompletableFuture<*> {
        val futures = worlds.map {
            val worldId = it.id
            val newRandomNumber = it.randomNumber
            dataSource
                .sendPreparedStatement(SELECT_WORLD, listOf(worldId))
                .thenApply { rowSet ->
                    val row = rowSet.rows.iterator().next()
                    row.getInt(1) // Read 'randomNumber' to comply with Test type 5, point 6
                    dataSource
                        .sendPreparedStatement(UPDATE_WORLD, listOf(newRandomNumber, worldId))
                }
        }.toTypedArray()

        return allOf(*futures).thenApply { futures.map { f -> f.get() ?: error("") } }
    }

    override fun initWorldsCache(cache: Cache<Int, CachedWorld>) {
        dataSource
            .sendPreparedStatement("select * from world")
            .thenApply { rowSet ->
                rowSet.rows.map { row ->
                    val id = row.getInt(0) ?: error("")
                    val randomNumber = row.getInt(1) ?: error("")
                    cache.put(id, CachedWorld(id, randomNumber))
                }
            }
            .get()
    }

    override fun loadCachedWorld(id: Int): CachedWorld =
        findWorld(id, dataSource)
            .thenApply { world -> CachedWorld(world.id, world.randomNumber) }
            .get()

    override fun close() {
        dataSource.disconnect().get()
    }

    private fun findWorld(id: Int, client: ConnectionPool<*>): CompletableFuture<World> =
        client
            .sendPreparedStatement(SELECT_WORLD, listOf(id))
            .thenApply { rowSet ->
                val row = rowSet.rows.iterator().next()
                val id1 = row.getInt(0) ?: error("")
                val randomNumber = row.getInt(1) ?: error("")
                World(id1, randomNumber)
            }
}
