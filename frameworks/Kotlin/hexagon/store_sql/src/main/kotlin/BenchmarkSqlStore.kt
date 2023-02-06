package com.hexagonkt.store

import com.hexagonkt.model.CachedWorld
import com.hexagonkt.model.Fortune
import com.hexagonkt.Settings
import com.hexagonkt.model.World
import com.hexagonkt.core.Jvm
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.cache2k.Cache
import java.sql.Connection
import java.sql.PreparedStatement

class BenchmarkSqlStore(
    engine: String, private val settings: Settings = Settings()
) : BenchmarkStore(settings) {

    companion object {
        private const val SELECT_WORLD: String = "select * from world where id = ?"
        private const val UPDATE_WORLD: String = "update world set randomNumber = ? where id = ?"
        private const val SELECT_ALL_FORTUNES: String = "select * from fortune"
    }

    private val dataSource: HikariDataSource by lazy {
        val dbHost = Jvm.systemSettingOrNull("${engine.uppercase()}_DB_HOST") ?: "localhost"
        val environment = Jvm.systemSettingOrNull(String::class, "BENCHMARK_ENV")?.lowercase()
        val poolSize = 8 + if (environment == "citrine") Jvm.cpuCount else Jvm.cpuCount * 2
        val postgresqlSettings = listOf(
            "ssl=false",
            "assumeMinServerVersion=12.10",
            "databaseMetadataCacheFieldsMiB=8",
            "prepareThreshold=1",
            "reWriteBatchedInserts=true",
        ).joinToString("&")
        val config = HikariConfig().apply {
            jdbcUrl = "jdbc:postgresql://$dbHost/${settings.databaseName}?$postgresqlSettings"
            maximumPoolSize = Jvm.systemSettingOrNull(Int::class, "maximumPoolSize") ?: poolSize
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
            val stmtUpdate = con.prepareStatement(UPDATE_WORLD)

            worlds.forEach {
                val worldId = it.id
                val newRandomNumber = it.randomNumber

                stmtSelect.setInt(1, worldId)
                val rs = stmtSelect.executeQuery()
                rs.next()
                rs.getInt(2) // Read 'randomNumber' to comply with Test type 5, point 6

                stmtUpdate.setInt(1, newRandomNumber)
                stmtUpdate.setInt(2, worldId)
                stmtUpdate.executeUpdate()
            }
        }
    }

    override fun initWorldsCache(cache: Cache<Int, CachedWorld>) {
        dataSource.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement("select * from world")
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
