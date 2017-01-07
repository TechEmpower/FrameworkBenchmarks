package co.there4.hexagon

import co.there4.hexagon.repository.MongoIdRepository
import co.there4.hexagon.repository.mongoCollection
import java.lang.System.getenv

import co.there4.hexagon.settings.SettingsManager.setting
import co.there4.hexagon.repository.mongoDatabase
import co.there4.hexagon.util.err
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import java.io.Closeable
import kotlin.reflect.KProperty1
import java.sql.Connection
import java.sql.ResultSet.CONCUR_READ_ONLY
import java.sql.ResultSet.TYPE_FORWARD_ONLY
import javax.sql.DataSource

internal val DB_ROWS = 10000

private val DB_HOST = getenv("DBHOST") ?: "localhost"
private val DB = setting<String>("database") ?: "hello_world"
private val WORLD: String = setting<String>("worldCollection") ?: "world"
private val FORTUNE: String = setting<String>("fortuneCollection") ?: "fortune"

internal fun createStore(engine: String): Repository = when (engine) {
    "mongodb" -> MongoDbRepository()
    "mysql" -> MySqlRepository()
    else -> error("Unsupported database")
}

internal interface Repository {
    fun findFortunes(): List<Fortune>
    fun findWorlds(queries: Int): List<World>
    fun replaceWorlds(queries: Int): List<World>
}

internal class MongoDbRepository : Repository {
    private val database = mongoDatabase("mongodb://$DB_HOST/$DB")

    internal val worldRepository = repository(WORLD, World::_id)
    internal val fortuneRepository = repository(FORTUNE, Fortune::_id)

    // TODO Find out why it fails when creating index '_id' with background: true
    private inline fun <reified T : Any> repository(name: String, key: KProperty1<T, Int>) =
        MongoIdRepository(T::class, mongoCollection(name, database), key, indexOrder = null)

    override fun findFortunes() = fortuneRepository.findObjects().toList()

    override fun findWorlds(queries: Int) =
        (1..queries).map { worldRepository.find(rnd()) }.filterNotNull()

    override fun replaceWorlds(queries: Int) = (1..queries).map {
        val id = rnd()
        val newWorld = World(id, id)
        worldRepository.replaceObject(newWorld)
        newWorld
    }
}

internal class MySqlRepository : Repository {
    private val SELECT_WORLD = "select * from world where id = ?"
    private val UPDATE_WORLD = "update world set randomNumber = ? where id = ?"
    private val SELECT_FORTUNES = "select * from fortune"

    private val DATA_SOURCE: DataSource

    init {
        val config = HikariConfig()
        config.jdbcUrl = "jdbc:mysql://root@$DB_HOST/$DB?useSSL=false"
        config.maximumPoolSize = 256
        DATA_SOURCE = HikariDataSource(config)
    }

    override fun findFortunes(): List<Fortune> {
        var fortunes = listOf<Fortune>()

        val connection = KConnection(DATA_SOURCE.connection ?: err)
        connection.use { con: Connection ->
            val rs = con.prepareStatement(SELECT_FORTUNES).executeQuery()
            while (rs.next())
                fortunes += Fortune(rs.getInt(1), rs.getString(2))
        }

        return fortunes
    }

    class KConnection(conn: Connection) : Connection by conn, Closeable

    override fun findWorlds(queries: Int): List<World> {
        var worlds: List<World> = listOf()

        KConnection(DATA_SOURCE.connection).use { con: Connection ->
            val stmtSelect = con.prepareStatement(SELECT_WORLD)

            for (ii in 0..queries - 1) {
                stmtSelect.setInt(1, rnd())
                val rs = stmtSelect.executeQuery()
                rs.next()
                worlds += World(rs.getInt(1), rs.getInt(2))
            }
        }

        return worlds
    }

    override fun replaceWorlds(queries: Int): List<World> {
        var worlds: List<World> = listOf()

        KConnection(DATA_SOURCE.connection).use { con: Connection ->
            con.autoCommit = false

            val stmtSelect = con.prepareStatement(SELECT_WORLD, TYPE_FORWARD_ONLY, CONCUR_READ_ONLY)
            val stmtUpdate = con.prepareStatement(UPDATE_WORLD)

            for (ii in 0..queries - 1) {
                stmtSelect.setInt(1, rnd())
                val rs = stmtSelect.executeQuery()
                rs.next()

                val world = World(rs.getInt(1), rs.getInt(2)).copy(randomNumber = rnd())
                worlds += world
                stmtUpdate.setInt(1, world.randomNumber)
                stmtUpdate.setInt(2, world.id)
                stmtUpdate.addBatch()

                if (ii % 75 == 0)
                    con.commit()
            }

            con.commit()
        }

        return worlds
    }
}
