import com.fasterxml.jackson.databind.JsonNode
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.http4k.format.Jackson.number
import org.http4k.format.Jackson.obj
import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.util.*
import javax.sql.DataSource

interface Database {
    fun findWorld(): JsonNode?
    fun findWorlds(count: Int): List<JsonNode>
    fun updateWorlds(count: Int): List<JsonNode>
    fun fortunes(): List<Fortune>
}

class BlockingDatabase private constructor(private val dataSource: DataSource) : Database {

    override fun findWorld() = withConnection {
        findWorld(randomWorld())
    }

    override fun findWorlds(count: Int) = withConnection {
        (1..count).mapNotNull { findWorld(randomWorld()) }
    }

    override fun updateWorlds(count: Int) = withConnection {
        (1..count).mapNotNull {
            val id = randomWorld()
            updateWorld(id)
            findWorld(id)
        }
    }

    override fun fortunes() = withConnection {
        val original = withStatement("select * from fortune") { executeQuery().toList { Fortune(getInt(1), getString(2)) } }
        (original + Fortune(0, "Additional fortune added at request time.")).sortedBy { it.message }
    }

    companion object {
        operator fun invoke(host: String): Database {
            val postgresqlUrl = "jdbc:postgresql://$host:5432/hello_world?" +
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

            val config = HikariConfig()
            config.jdbcUrl = postgresqlUrl
            config.maximumPoolSize = 100
            config.username = "benchmarkdbuser"
            config.password = "benchmarkdbpass"
            return BlockingDatabase(HikariDataSource(config))
        }
    }

    private fun <T> withConnection(fn: Connection.() -> T): T = dataSource.connection.use(fn)

    private fun <T> Connection.withStatement(stmt: String, fn: PreparedStatement.() -> T): T = prepareStatement(stmt).use(fn)

    private fun Connection.updateWorld(id: Int) = withStatement("UPDATE world SET randomNumber = ? WHERE id = ?") {
        setInt(1, randomWorld())
        setInt(2, id)
        executeUpdate()
    }

    private fun Connection.findWorld(id: Int) =
            withStatement("SELECT * FROM world WHERE id = ?") {
                setInt(1, id)
                executeQuery().toList {
                    obj("id" to number(getInt("id")), "randomNumber" to number(getInt("randomNumber")))
                }.firstOrNull()
            }

    private fun <T> ResultSet.toList(fn: ResultSet.() -> T): List<T> =
            use {
                mutableListOf<T>().apply {
                    while (next()) {
                        add(fn(this@toList))
                    }
                }
            }
}

private fun randomWorld() = Random().nextInt(9999) + 1
