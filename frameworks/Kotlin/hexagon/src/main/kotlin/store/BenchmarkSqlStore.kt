package com.hexagonkt.store

import com.hexagonkt.CachedWorld
import com.hexagonkt.Fortune
import com.hexagonkt.Settings
import com.hexagonkt.World
import com.hexagonkt.helpers.Jvm
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.cache2k.Cache
import java.sql.Connection
import java.sql.PreparedStatement

internal class BenchmarkSqlStore(engine: String, private val settings: Settings = Settings())
    : BenchmarkStore(settings) {

    companion object {
        private const val SELECT_WORLD = "select * from world where id = ?"
        private const val UPDATE_WORLD = "update world set randomNumber = ? where id = ?"
        private const val SELECT_ALL_FORTUNES = "select * from fortune"
    }

    private val dbHost: String by lazy { Jvm.systemSetting("${engine.uppercase()}_DB_HOST") ?: "localhost" }
    private val jdbcUrl: String by lazy { "jdbc:postgresql://$dbHost/${settings.databaseName}" }
    private val dataSource: HikariDataSource by lazy {
        val config = HikariConfig()
        config.jdbcUrl = jdbcUrl
        config.maximumPoolSize = Jvm.systemSetting(Int::class, "maximumPoolSize") ?: 64
        config.username = Jvm.systemSetting("databaseUsername") ?: "benchmarkdbuser"
        config.password = Jvm.systemSetting("databasePassword") ?: "benchmarkdbpass"
        HikariDataSource(config)
    }

    override fun findAllFortunes(): List<Fortune> {
        val fortunes = mutableListOf<Fortune>()

        dataSource.connection.use { con: Connection ->
            val rs = con.prepareStatement(SELECT_ALL_FORTUNES).executeQuery()
            while (rs.next())
                fortunes += Fortune(rs.getInt(1), rs.getString(2))
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

    private fun Connection.findWorld(id: Int, stmtSelect: PreparedStatement = prepareStatement(SELECT_WORLD)): World {
        stmtSelect.setInt(1, id)
        val rs = stmtSelect.executeQuery()
        rs.next()
        return World(rs.getInt(1), rs.getInt(2))
    }
}