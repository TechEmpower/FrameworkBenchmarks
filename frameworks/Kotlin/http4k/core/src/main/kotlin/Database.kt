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
import java.sql.ResultSet
import java.util.*
import java.util.concurrent.CompletableFuture
import javax.sql.DataSource

interface Database {
    fun findWorld(): JsonNode?
    fun findWorlds(count: Int): List<JsonNode>
    fun updateWorlds(count: Int): List<JsonNode>
    fun fortunes(): List<Fortune>
}

class PostgresDatabase private constructor(private val dataSource: DataSource) : Database {

    override fun findWorld() = findWorlds(1).first()

    override fun findWorlds(count: Int) = withConnection {
        findWorlds(count,
                { getInt("randomNumber") },
                { id, rn -> obj("id" to number(id), "randomNumber" to number(rn)) })
    }

    override fun updateWorlds(count: Int) = withConnection {
        withConnection {
            val worlds = findWorlds(count, { randomWorld() }, { id, rn -> id to rn })

            withStatement("UPDATE world SET randomNumber = ? WHERE id = ?") {
                worlds.forEach { (id: Int, rn: Int) ->
                    setInt(1, rn)
                    setInt(2, id)
                    executeUpdate()
                }
            }

            worlds.map { obj("id" to number(it.first), "randomNumber" to number(it.second)) }
        }
    }

    override fun fortunes() = withConnection {
        val original = withStatement("select * from fortune") { executeQuery().toResultsList { Fortune(getInt(1), getString(2)) } }
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
                maximumPoolSize = 100
                HikariDataSource(this)
            }
            return PostgresDatabase(dataSource)
        }
    }

    private fun <T> withConnection(fn: Connection.() -> T): T = dataSource.connection.use(fn)

    private fun <T> Connection.withStatement(stmt: String, fn: PreparedStatement.() -> T): T = prepareStatement(stmt).use(fn)

    private fun <T> Connection.findWorlds(count: Int, randomNumberFn: ResultSet.() -> Int, transform: (Int, Int) -> T) =
            withStatement("SELECT * FROM world WHERE id = ?") {
                (1..count).map {
                    setInt(1, it)
                    executeQuery().toResultsList {
                        transform(getInt("id"), randomNumberFn())
                    }.first()
                }
            }

    private fun <T> ResultSet.toResultsList(fn: ResultSet.() -> T): List<T> =
            use {
                mutableListOf<T>().apply {
                    while (next()) {
                        add(fn(this@toResultsList))
                    }
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
                maxSize = 100
                cachePreparedStatements = true
            }
            return ReactivePostgresDatabase(pool(PgPoolOptions(options)))
        }
    }

    override fun findWorld(): JsonNode? {
        val deferred = CompletableFuture<JsonNode?>()
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
