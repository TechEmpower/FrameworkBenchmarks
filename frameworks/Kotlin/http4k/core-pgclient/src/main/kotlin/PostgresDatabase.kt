import io.vertx.core.Vertx
import io.vertx.core.VertxOptions
import io.vertx.pgclient.PgConnectOptions
import io.vertx.pgclient.PgPool.client
import io.vertx.sqlclient.PoolOptions
import io.vertx.sqlclient.SqlClient
import io.vertx.sqlclient.Tuple
import org.http4k.format.Argo.number
import org.http4k.format.Argo.obj

class PostgresDatabase : Database {
    private val queryPool: SqlClient
    private val updatePool: SqlClient

    init {
        val vertx = Vertx.vertx(VertxOptions().setPreferNativeTransport(true))
        val connectOptions = PgConnectOptions().apply {
            port = 5432
            cachePreparedStatements = true
            host = "tfb-database"
            database = "hello_world"
            user = "benchmarkdbuser"
            password = "benchmarkdbpass"
        }
        val clientOptions = PoolOptions().setMaxSize(64)
        queryPool = client(vertx, connectOptions, clientOptions)
        updatePool = client(vertx, connectOptions, clientOptions)
    }

    override fun findWorld() =
        findWorld(randomWorld(), queryPool).map { it.toJson() }.toCompletionStage().toCompletableFuture().get()

    override fun loadAll() = queryPool.preparedQuery("SELECT id, randomnumber FROM world ")
        .execute()
        .map {
            it.associate {
                it.getInteger("id") to (it.getInteger("id") to it.getInteger("randomnumber")).toJson()
            }
        }
        .toCompletionStage().toCompletableFuture().get()

    override fun findWorlds(count: Int) =
        (1..count).map {
            findWorld(randomWorld(), queryPool)
                .map { it.toJson() }.toCompletionStage().toCompletableFuture().get()
        }

    override fun updateWorlds(count: Int) =
        (1..count)
            .map { randomWorld() to randomWorld() }
            .map { update ->
                updatePool.preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
                    .execute(Tuple.of(update.first, update.second))
                    .flatMap { findWorld(update.first, queryPool).map { it.toJson() } }
                    .toCompletionStage().toCompletableFuture().get()
            }

    override fun fortunes() = queryPool.preparedQuery("SELECT id, message FROM fortune")
        .execute()
        .map { it.map { Fortune(it.getInteger(0), it.getString(1)) } }
        .map { (it + Fortune(0, "Additional fortune added at request time.")) }
        .toCompletionStage().toCompletableFuture().get()
        .sortedBy { it.message }

    companion object {
        private fun findWorld(id: Int, pool: SqlClient) =
            pool.preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                .execute(Tuple.of(id))
                .map { rows ->
                    val r = rows.iterator().next()
                    r.getInteger("id") to r.getInteger("randomnumber")
                }
    }
}

private fun Pair<Int, Int>.toJson() = obj("id" to number(first), "randomNumber" to number(second))
