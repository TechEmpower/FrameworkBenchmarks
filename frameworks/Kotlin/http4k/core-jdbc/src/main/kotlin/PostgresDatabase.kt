import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import java.sql.Connection
import java.sql.ResultSet
import javax.sql.DataSource

class PostgresDatabase private constructor(private val dataSource: DataSource) : Database {

    override fun findWorld() = withConnection { findWorld(randomWorld()) }

    override fun loadAll() = withConnection {
        executeQuery("SELECT id, randomNumber FROM world") { it.toResultsList(::toWorld) }
    }

    override fun findWorlds(count: Int) = withConnection {
        (1..count).map { findWorld(randomWorld()) }
    }

    override fun updateWorlds(count: Int) = withConnection {
        val updated = (1..count).map { findWorld(it).first to randomWorld() }
        val stmt = createStatement()
        updated.forEach { (id, random) ->
            stmt.addBatch("UPDATE world SET randomNumber = $id WHERE id = $random")
        }
        stmt.executeBatch()
        stmt.close()
        updated
    }

    override fun fortunes() = withConnection {
        val original = executeQuery("select * from fortune") { it.toResultsList(::toFortune) }
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

    private fun Connection.findWorld(id: Int) =
        executeQuery("SELECT id, randomNumber FROM world WHERE id = $id") {
            it.toResultsList(::toWorld).first()
        }

    private inline fun <T> ResultSet.toResultsList(fn: (ResultSet) -> T): List<T> =
        mutableListOf<T>().apply {
            while (next()) add(fn(this@toResultsList))
        }
}

private inline fun <T> Connection.executeQuery(stmt: String, fn: (ResultSet) -> T): T =
    prepareStatement(stmt).use { fn(it.executeQuery()) }

private fun toFortune(it: ResultSet) = Fortune(it.getInt(1), it.getString(2))

private fun toWorld(resultSet: ResultSet) = resultSet.getInt("id") to resultSet.getInt("randomNumber")
