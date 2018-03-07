import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet

class Database(private val dataSource: javax.sql.DataSource) {

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
            return Database(HikariDataSource(config))
        }
    }

    fun <T> withConnection(fn: Connection.() -> T): T = dataSource.connection.use(fn)

    fun <T> withStatement(stmt: String, fn: PreparedStatement.() -> T): T = withConnection { withStatement(stmt, fn) }
}

fun <T> Connection.withStatement(stmt: String, fn: PreparedStatement.() -> T): T = prepareStatement(stmt).use(fn)

fun <T> ResultSet.toList(fn: ResultSet.() -> T): List<T> =
    use {
        mutableListOf<T>().apply {
            while (next()) {
                add(fn(this@toList))
            }
        }
    }
