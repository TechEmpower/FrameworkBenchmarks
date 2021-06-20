package com.hexagonkt.store

import com.hexagonkt.Fortune
import com.hexagonkt.Settings
import com.hexagonkt.World
import com.hexagonkt.helpers.Jvm
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import java.sql.Connection

internal class BenchmarkSqlStore(engine: String, private val settings: Settings = Settings()) : BenchmarkStore {
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

    override fun findWorlds(count: Int): List<World> {
        val worlds: MutableList<World> = mutableListOf()

        dataSource.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD)

            for (ii in 0 until count) {
                stmtSelect.setInt(1, randomWorld(settings.worldRows))
                val rs = stmtSelect.executeQuery()
                rs.next()
                val id = rs.getInt(1)
                worlds += World(id, id, rs.getInt(2))
            }
        }

        return worlds
    }

    override fun replaceWorlds(count: Int): List<World> {
        val worlds: MutableList<World> = mutableListOf()

        dataSource.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD)
            val stmtUpdate = con.prepareStatement(UPDATE_WORLD)

            for (ii in 0 until count) {
                val worldId = randomWorld(settings.worldRows)
                val newRandomNumber = randomWorld(settings.worldRows)

                stmtSelect.setInt(1, worldId)
                val rs = stmtSelect.executeQuery()
                rs.next()
                rs.getInt(2) // Read 'randomNumber' to comply with Test type 5, point 6

                worlds += World(worldId, worldId, newRandomNumber)

                stmtUpdate.setInt(1, newRandomNumber)
                stmtUpdate.setInt(2, worldId)
                stmtUpdate.executeUpdate()
            }
        }

        return worlds
    }

    override fun close() {
        dataSource.close()
    }
}