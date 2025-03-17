package com.hexagontk.store

import com.hexagontk.model.CachedWorld
import com.hexagontk.model.Fortune
import com.hexagontk.Settings
import com.hexagontk.model.World
import com.hexagontk.core.Platform
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.cache2k.Cache
import java.sql.Connection
import java.sql.PreparedStatement

class BenchmarkSqlStore(
    engine: String, private val settings: Settings = Settings()
) : BenchmarkStore(settings) {

    companion object {
        private const val LOAD_WORLDS: String = "select id, randomNumber from world"
        private const val SELECT_WORLD: String = "select id, randomNumber from world where id = ?"
        private const val UPDATE_WORLD: String = "update world set randomNumber = ? where id = ?"
        private const val SELECT_ALL_FORTUNES: String = "select id, message from fortune"
    }

    private val dataSource: HikariDataSource by lazy {
        val dbHost = Platform.systemSettingOrNull("${engine.uppercase()}_DB_HOST") ?: "tfb-database"
        val environment = Platform.systemSettingOrNull(String::class, "BENCHMARK_ENV")?.lowercase()
        val poolSize = 8 + if (environment == "citrine") Platform.cpuCount else Platform.cpuCount * 2
        val postgresqlSettings = listOf(
            "ssl=false",
            "assumeMinServerVersion=12.10",
            "databaseMetadataCacheFieldsMiB=8",
            "prepareThreshold=1",
            "reWriteBatchedInserts=true",
        ).joinToString("&")
        val config = HikariConfig().apply {
            jdbcUrl = "jdbc:postgresql://$dbHost/${settings.databaseName}?$postgresqlSettings"
            maximumPoolSize = Platform.systemSettingOrNull(Int::class, "maximumPoolSize") ?: poolSize
            driverClassName = settings.databaseDriver
            username = settings.databaseUsername
            password = settings.databasePassword
        }
        HikariDataSource(config)
    }

    override fun findAllFortunes(): List<Fortune> {
        var fortunes = listOf<Fortune>()

        dataSource.connection.use { con: Connection ->
            val rs = con.prepareStatement(SELECT_ALL_FORTUNES).executeQuery()
            while (rs.next())
                fortunes = fortunes + Fortune(rs.getInt(1), rs.getString(2))
        }

        return fortunes
    }

    override fun findWorlds(ids: List<Int>): List<World> =
        dataSource.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD)
            ids.map { con.findWorld(it, stmtSelect) }
        }

    override fun replaceWorlds(worlds: List<World>) {
        dataSource.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD)
            worlds.forEach {
                stmtSelect.setInt(1, it.id)
                val rs = stmtSelect.executeQuery()
                rs.next()
                rs.getInt(2) // Read 'randomNumber' to comply with Test type 5, point 6
            }

            val stmtUpdate = con.prepareStatement(UPDATE_WORLD)
            worlds.forEach {
                stmtUpdate.setInt(1, it.randomNumber)
                stmtUpdate.setInt(2, it.id)
                stmtUpdate.executeUpdate()
            }
        }
    }

    override fun initWorldsCache(cache: Cache<Int, CachedWorld>) {
        dataSource.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(LOAD_WORLDS)
            val rs = stmtSelect.executeQuery()

            while (rs.next()) {
                val id = rs.getInt(1)
                val randomNumber = rs.getInt(2)
                cache.put(id, CachedWorld(id, randomNumber))
            }
        }
    }

    override fun loadCachedWorld(id: Int): CachedWorld =
        dataSource.connection.use { con ->
            con.findWorld(id).let { world -> CachedWorld(world.id, world.randomNumber) }
        }

    override fun close() {
        dataSource.close()
    }

    private fun Connection.findWorld(
        id: Int, stmtSelect: PreparedStatement = prepareStatement(SELECT_WORLD)
    ): World {
        stmtSelect.setInt(1, id)
        val rs = stmtSelect.executeQuery()
        rs.next()
        return World(rs.getInt(1), rs.getInt(2))
    }
}
