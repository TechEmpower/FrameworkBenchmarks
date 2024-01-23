import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import java.sql.Connection
import java.sql.ResultSet
import java.util.Random
import javax.sql.DataSource

class PostgresDatabase private constructor(private val dataSource: DataSource) : Database {
    private val random = Random()

    override fun findWorld() = withConnection { findWorld(random.world()) }

    override fun loadAll() = withConnection {
        executeQuery("SELECT id, randomNumber FROM world") { it.toResultsList(::toWorld) }
    }

    override fun findWorlds(count: Int) = withConnection {
        (1..count).map { findWorld(random.world()) }
    }

    override fun updateWorlds(count: Int) = withConnection {
        val updatedAndSorted = (1..count)
            .map { findWorld(random.world()).first to random.world() }
            .sortedBy { it.first }

        createStatement().use { stmt ->
            updatedAndSorted.forEach {
                stmt.addBatch("UPDATE world SET randomNumber = ${it.second} WHERE id = ${it.first}")
            }
            stmt.executeBatch()
        }
        updatedAndSorted
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
}

private fun Connection.findWorld(id: Int) = prepareStatement("SELECT id, randomNumber FROM world WHERE id = ?").use {
    toWorld(it.apply { setInt(1, id) }.executeQuery().also { it.next() })
}

private inline fun <T> ResultSet.toResultsList(fn: (ResultSet) -> T): List<T> =
    mutableListOf<T>().apply {
        while (next()) add(fn(this@toResultsList))
    }

private inline fun <T> Connection.executeQuery(stmt: String, fn: (ResultSet) -> T): T =
    prepareStatement(stmt).use { fn(it.executeQuery()) }

private fun toFortune(it: ResultSet) = Fortune(it.getInt(1), it.getString(2))

private fun toWorld(resultSet: ResultSet) = resultSet.getInt("id") to resultSet.getInt("randomNumber")
