import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.http4k.format.Argo.number
import org.http4k.format.Argo.obj
import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import javax.sql.DataSource

class PostgresDatabase private constructor(private val dataSource: DataSource) : Database {

    override fun findWorld() = withConnection { findWorld(randomWorld()) }

    override fun loadAll() = withConnection { findAll() }

    override fun findWorlds(count: Int) = withConnection {
        (1..count).map { findWorld(randomWorld()) }
    }

    override fun updateWorlds(count: Int) = withConnection {
        (1..count).map {
            val id = randomWorld()
            updateWorld(id)
            findWorld(id)
        }
    }

    private fun Connection.updateWorld(id: Int) = withStatement("UPDATE world SET randomNumber = ? WHERE id = ?") {
        setInt(1, randomWorld())
        setInt(2, id)
        executeUpdate()
    }

    override fun fortunes() = withConnection {
        val original =
            withStatement("select * from fortune") { executeQuery().toResultsList { Fortune(getInt(1), getString(2)) } }
        (original + Fortune(0, "Additional fortune added at request time.")).sortedBy { it.message }
    }

    companion object {
        operator fun invoke(): Database =
            PostgresDatabase(HikariConfig().run {
                username = "benchmarkdbuser"
                password = "benchmarkdbpass"
                jdbcUrl = "jdbc:postgresql://tfb-database:5432/hello_world?" +
                    "useSSL=false&" +
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
                maximumPoolSize = 100
                HikariDataSource(this)
            })
    }

    private inline fun <T> withConnection(fn: Connection.() -> T): T = dataSource.connection.use(fn)

    private inline fun <T> Connection.withStatement(stmt: String, fn: PreparedStatement.() -> T): T =
        prepareStatement(stmt).use(fn)

    private fun Connection.findWorld(id: Int) =
        withStatement("SELECT id, randomNumber FROM world WHERE id = ?") {
            setInt(1, id)
            executeQuery().toResultsList {
                obj("id" to number(getInt("id")), "randomNumber" to number(getInt("randomNumber")))
            }.first()
        }

    private fun Connection.findAll() =
        withStatement("SELECT id, randomNumber FROM world") {
            executeQuery().toResultsList {
                val id = getInt("id")
                id to obj("id" to number(id), "randomNumber" to number(getInt("randomNumber")))
            }.toMap()
        }

    private inline fun <T> ResultSet.toResultsList(fn: ResultSet.() -> T): List<T> =
        mutableListOf<T>().apply {
            while (next()) {
                add(fn(this@toResultsList))
            }
        }
}