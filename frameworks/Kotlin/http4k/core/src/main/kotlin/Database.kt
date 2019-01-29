import com.fasterxml.jackson.databind.JsonNode
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import io.reactiverse.pgclient.PgClient.pool
import io.reactiverse.pgclient.PgPool
import io.reactiverse.pgclient.PgPoolOptions
import io.reactiverse.pgclient.Tuple
import org.http4k.format.Jackson.number
import org.http4k.format.Jackson.obj
import java.sql.Connection
import java.sql.PreparedStatement
import java.util.Random
import java.util.concurrent.CompletableFuture
import javax.sql.DataSource

interface Database {
    fun findWorld(): JsonNode
    fun findWorlds(count: Int): List<JsonNode>
    fun updateWorlds(count: Int): List<JsonNode>
    fun fortunes(): List<Fortune>
}

class PostgresDatabase private constructor(private val dataSource: DataSource) : Database {

    override fun findWorld() = withConnection {
        findWorld(randomWorld())
    }

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
        val original = withStatement("select * from fortune") {
            with(executeQuery()) {
                mutableListOf<Fortune>().apply {
                    while (next()) {
                        add(Fortune(getInt(1), getString(2)))
                    }
                }
            }
        }
        (original + Fortune(0, "Additional fortune added at request time.")).sortedBy { it.message }
    }

    companion object {
        operator fun invoke(host: String): Database {
            val dataSource = HikariConfig().run {
                username = "benchmarkdbuser"
                password = "benchmarkdbpass"
                jdbcUrl = "jdbc:postgresql://$host:5432/hello_world?" +
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
                maximumPoolSize = Runtime.getRuntime().availableProcessors() * 2
                HikariDataSource(this)
            }
            return PostgresDatabase(dataSource)
        }
    }

    private inline fun <T> withConnection(fn: Connection.() -> T): T = dataSource.connection.use(fn)

    private inline fun <T> Connection.withStatement(stmt: String, fn: PreparedStatement.() -> T): T = prepareStatement(stmt).use(fn)

    private fun Connection.findWorld(id: Int) =
        withStatement("SELECT * FROM world WHERE id = ?") {
            setInt(1, id)
            with(executeQuery()) {
                next()
                obj("id" to number(getInt("id")), "randomNumber" to number(getInt("randomNumber")))
            }
        }
}

class ReactivePostgresDatabase private constructor(private val db: PgPool) : Database {
    companion object {
        operator fun invoke(hostName: String): Database {
            val options = PgPoolOptions().apply {
                database = "hello_world"
                host = hostName
                port = 5432
                user = "benchmarkdbuser"
                password = "benchmarkdbpass"
                maxSize = Runtime.getRuntime().availableProcessors() * 2
                cachePreparedStatements = true
            }
            return ReactivePostgresDatabase(pool(PgPoolOptions(options)))
        }
    }

    override fun findWorld(): JsonNode {
        val deferred = CompletableFuture<JsonNode>()
        db.preparedQuery("SELECT id, randomnumber from WORLD where id=$1", Tuple.of(randomWorld())) {
            with(it.result().first()) {
                deferred.complete(obj("id" to number(getInteger(0)), "randomNumber" to number(getInteger(1))))
            }
        }
        return deferred.get()
    }

    override fun findWorlds(count: Int): List<JsonNode> {
        val deferred = CompletableFuture<List<JsonNode>>()
        val worlds = mutableListOf<JsonNode>()

        (1..count).forEach {
            db.preparedQuery("SELECT id, randomnumber from WORLD where id=$1", Tuple.of(randomWorld())) {
                with(it.result().first()) {
                    worlds.add(obj("id" to number(getInteger(0)), "randomNumber" to number(getInteger(1))))
                }
                if (worlds.size == count) deferred.complete(worlds)
            }
        }

        return deferred.get()
    }

    override fun updateWorlds(count: Int): List<JsonNode> {
        val deferred = CompletableFuture<List<JsonNode>>()
        val worlds = mutableListOf<Tuple>()
        (1..count).forEach {
            db.preparedQuery("SELECT id from WORLD where id=$1", Tuple.of(randomWorld())) { ar ->
                with(ar.result().first()) {
                    worlds.add(Tuple.of(getInteger(0), randomWorld()))

                    if (worlds.size == count) {
                        db.preparedBatch("UPDATE world SET randomnumber=$1 WHERE id=$2", worlds) {
                            deferred.complete(worlds.map {
                                obj("id" to number(it.getInteger(0)), "randomNumber" to number(it.getInteger(1)))
                            })
                        }
                    }
                }
            }
        }
        return deferred.get()
    }

    override fun fortunes(): List<Fortune> {
        val deferred = CompletableFuture<List<Fortune>>()
        val fortunes = mutableListOf<Fortune>()

        db.preparedQuery("SELECT id, message from FORTUNE") {
            with(it.result().iterator()) {
                while (hasNext()) {
                    with(next()) { fortunes.add(Fortune(getInteger(0), getString(1))) }
                }
                deferred.complete(fortunes + Fortune(0, "Additional fortune added at request time."))
            }
        }

        return deferred.get().sortedBy { it.message }
    }
}

private fun randomWorld() = Random().nextInt(9999) + 1
