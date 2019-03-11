package com.hexagonkt

import com.hexagonkt.helpers.error
import com.hexagonkt.helpers.Jvm.systemSetting
import com.hexagonkt.settings.SettingsManager.defaultSetting
import com.hexagonkt.store.mongodb.MongoDbStore

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource

import java.sql.Connection
import java.util.concurrent.ThreadLocalRandom

internal const val WORLD_ROWS: Int = 10000

private val worldName: String = defaultSetting("worldCollection", "world")
private val fortuneName: String = defaultSetting("fortuneCollection", "fortune")
private val databaseName: String = defaultSetting("database", "hello_world")

internal fun randomWorld(): Int = ThreadLocalRandom.current().nextInt(WORLD_ROWS) + 1

internal interface BenchmarkStore {
    fun findAllFortunes(): List<Fortune>
    fun findWorlds(count: Int): List<World>
    fun replaceWorlds(count: Int): List<World>
    fun close()
}

internal class BenchmarkMongoDbStore(engine: String) : BenchmarkStore {

    private val dbHost: String = systemSetting("${engine.toUpperCase()}_DB_HOST", "localhost")

    private val dbUrl: String = "mongodb://$dbHost/$databaseName"

    private val worldRepository by lazy {
        MongoDbStore(World::class, World::_id, dbUrl, worldName)
    }

    private val fortuneRepository by lazy {
        MongoDbStore(Fortune::class, Fortune::_id, dbUrl, fortuneName)
    }

    override fun findAllFortunes(): List<Fortune> = fortuneRepository.findAll()

    override fun findWorlds(count: Int): List<World> =
        (1..count).mapNotNull { worldRepository.findOne(randomWorld()) }

    override fun replaceWorlds(count: Int): List<World> = (1..count)
        .map {
            val world = worldRepository.findOne(randomWorld()) ?: error
            val worldCopy = world.copy(randomNumber = randomWorld())
            worldRepository.replaceOne(worldCopy)
            worldCopy
        }

    override fun close() { /* Not needed */ }
}

internal class BenchmarkSqlStore(engine: String) : BenchmarkStore {
    companion object {
        private const val SELECT_WORLD = "select * from world where id = ?"
        private const val UPDATE_WORLD = "update world set randomNumber = ? where id = ?"
        private const val SELECT_ALL_FORTUNES = "select * from fortune"
    }

    private val dbHost: String = systemSetting("${engine.toUpperCase()}_DB_HOST", "localhost")

    private val jdbcUrl: String = "jdbc:postgresql://$dbHost/$databaseName"

    private val dataSource: HikariDataSource by lazy {
        val config = HikariConfig()
        config.jdbcUrl = jdbcUrl
        config.maximumPoolSize = defaultSetting("maximumPoolSize", 64)
        config.username = defaultSetting("databaseUsername", "benchmarkdbuser")
        config.password = defaultSetting("databasePassword", "benchmarkdbpass")
        HikariDataSource(config)
    }

    override fun findAllFortunes(): List<Fortune> {
        var fortunes = listOf<Fortune>()

        dataSource.connection.use { con: Connection ->
            val rs = con.prepareStatement(SELECT_ALL_FORTUNES).executeQuery()
            while (rs.next())
                fortunes += Fortune(rs.getInt(1), rs.getString(2))
        }

        return fortunes
    }

    override fun findWorlds(count: Int): List<World> {
        var worlds: List<World> = listOf()

        dataSource.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD)

            for (ii in 0 until count) {
                stmtSelect.setInt(1, randomWorld())
                val rs = stmtSelect.executeQuery()
                rs.next()
                val id = rs.getInt(1)
                worlds += World(id, id, rs.getInt(2))
            }
        }

        return worlds
    }

    override fun replaceWorlds(count: Int): List<World> {
        var worlds: List<World> = listOf()

        dataSource.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD)
            val stmtUpdate = con.prepareStatement(UPDATE_WORLD)

            for (ii in 0 until count) {
                val worldId = randomWorld()
                val newRandomNumber = randomWorld()

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
