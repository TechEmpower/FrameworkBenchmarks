package co.there4.hexagon

import co.there4.hexagon.settings.SettingsManager.settings
import co.there4.hexagon.store.MongoIdRepository
import co.there4.hexagon.store.mongoCollection
import co.there4.hexagon.store.mongoDatabase

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource

import java.lang.System.getenv
import java.sql.Connection
import java.sql.ResultSet.CONCUR_READ_ONLY
import java.sql.ResultSet.TYPE_FORWARD_ONLY
import javax.sql.DataSource

import kotlin.reflect.KProperty1

internal const val WORLD_ROWS = 10000

private val DB_HOST = getenv("DBHOST") ?: "localhost"
private val DB_NAME = settings["database"] as? String ?: "hello_world"
private val WORLD_NAME: String = settings["worldCollection"] as? String ?: "world"
private val FORTUNE_NAME: String = settings["fortuneCollection"] as? String ?: "fortune"

private val postgresqlUrl = "jdbc:postgresql://$DB_HOST/$DB_NAME?" +
    "jdbcCompliantTruncation=false&" +
    "elideSetAutoCommits=true&" +
    "useLocalSessionState=true&" +
    "cachePrepStmts=true&" +
    "cacheCallableStmts=true&" +
    "alwaysSendSetIsolation=false&" +
    "prepStmtCacheSize=4096&" +
    "cacheServerConfiguration=true&" +
    "prepStmtCacheSqlLimit=2048&" +
    "traceProtocol=false&" +
    "useUnbufferedInput=false&" +
    "useReadAheadInput=false&" +
    "maintainTimeStats=false&" +
    "useServerPrepStmts=true&" +
    "cacheRSMetadata=true"

internal fun createStore(engine: String): Store = when (engine) {
    "mongodb" -> MongoDbStore()
    "postgresql" -> SqlStore(postgresqlUrl)
    else -> error("Unsupported database")
}

internal interface Store {
    fun findAllFortunes(): List<Fortune>
    fun findWorlds(count: Int): List<World>
    fun replaceWorlds(count: Int): List<World>
}

private class MongoDbStore : Store {
    private val database = mongoDatabase("mongodb://$DB_HOST/$DB_NAME")

    private val worldRepository = repository(WORLD_NAME, World::_id)
    private val fortuneRepository = repository(FORTUNE_NAME, Fortune::_id)

    // TODO Find out why it fails when creating index '_id' with background: true
    private inline fun <reified T : Any> repository(name: String, key: KProperty1<T, Int>) =
        MongoIdRepository(T::class, mongoCollection(name, database), key, indexOrder = null)

    override fun findAllFortunes() = fortuneRepository.findObjects().toList()

    override fun findWorlds(count: Int) =
        (1..count).map { worldRepository.find(randomWorld()) }.filterNotNull()

    override fun replaceWorlds(count: Int) = (1..count)
        .map { worldRepository.find(randomWorld())?.copy(randomNumber = randomWorld()) }
        .toList()
        .filterNotNull()
        .map {
            worldRepository.replaceObjects(it, bulk = true)
            it
        }
}

private class SqlStore(jdbcUrl: String) : Store {
    private val SELECT_WORLD = "select * from world where id = ?"
    private val UPDATE_WORLD = "update world set randomNumber = ? where id = ?"
    private val SELECT_ALL_FORTUNES = "select * from fortune"

    private val DATA_SOURCE: DataSource

    init {
        val config = HikariConfig()
        config.jdbcUrl = jdbcUrl
        config.maximumPoolSize =  settings["maximumPoolSize"] as? Int ?: 32
        config.username = settings["databaseUsername"] as? String ?: "benchmarkdbuser"
        config.password = settings["databasePassword"] as? String ?:  "benchmarkdbpass"
        DATA_SOURCE = HikariDataSource(config)
    }

    override fun findAllFortunes(): List<Fortune> {
        var fortunes = listOf<Fortune>()

        DATA_SOURCE.connection.use { con: Connection ->
            val rs = con.prepareStatement(SELECT_ALL_FORTUNES).executeQuery()
            while (rs.next())
                fortunes += Fortune(rs.getInt(1), rs.getString(2))
        }

        return fortunes
    }

    override fun findWorlds(count: Int): List<World> {
        var worlds: List<World> = listOf()

        DATA_SOURCE.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD)

            for (ii in 0..count - 1) {
                stmtSelect.setInt(1, randomWorld())
                val rs = stmtSelect.executeQuery()
                rs.next()
                val _id = rs.getInt(1)
                worlds += World(_id, _id, rs.getInt(2))
            }
        }

        return worlds
    }

    override fun replaceWorlds(count: Int): List<World> {
        var worlds: List<World> = listOf()

        DATA_SOURCE.connection.use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD, TYPE_FORWARD_ONLY, CONCUR_READ_ONLY)
            val stmtUpdate = con.prepareStatement(UPDATE_WORLD)

            for (ii in 0..count - 1) {
                stmtSelect.setInt(1, randomWorld())
                val rs = stmtSelect.executeQuery()
                rs.next()

                val _id = rs.getInt(1)
                val world = World(_id, _id, rs.getInt(2)).copy(randomNumber = randomWorld())
                worlds += world

                stmtUpdate.setInt(1, world.randomNumber)
                stmtUpdate.setInt(2, world.id)
                stmtUpdate.addBatch()

                if (ii % 25 == 0)
                    stmtUpdate.executeBatch()
            }

            stmtUpdate.executeBatch()
        }

        return worlds
    }
}
