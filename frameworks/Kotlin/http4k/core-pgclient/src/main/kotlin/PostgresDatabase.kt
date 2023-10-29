import io.vertx.core.Future
import io.vertx.core.Vertx
import io.vertx.core.VertxOptions
import io.vertx.pgclient.PgConnectOptions
import io.vertx.pgclient.PgPool.client
import io.vertx.sqlclient.PoolOptions
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.SqlClient
import io.vertx.sqlclient.Tuple
import java.util.Random

class PostgresDatabase : Database {
    private val queryPool: SqlClient
    private val updatePool: SqlClient

    private val random = Random()

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
        findWorld(random.world(), queryPool).toCompletionStage().toCompletableFuture().get()

    override fun loadAll() = queryPool.preparedQuery("SELECT id, randomnumber FROM world ")
        .execute()
        .map { it.map(::toWorld) }
        .toCompletionStage().toCompletableFuture().get()

    override fun findWorlds(count: Int) =
        (1..count).map {
            findWorld(random.world(), queryPool)
                .toCompletionStage().toCompletableFuture().get()
        }

    override fun updateWorlds(count: Int): List<Pair<Int, Int>> {
        val updatedAndSorted = Future
            .all(
                (1..count)
                    .map {
                        findWorld(random.world(), queryPool)
                            .map { it.first to random.world() }
                    }
            )
            .toCompletionStage().toCompletableFuture().get().list<World>()
            .sortedBy { it.first }

        updatePool.preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
            .executeBatch(updatedAndSorted.map { Tuple.of(it.first, it.second) })
            .toCompletionStage().toCompletableFuture().get()

        return updatedAndSorted
    }

    override fun fortunes() = queryPool.preparedQuery("SELECT id, message FROM fortune")
        .execute()
        .map { it.map(::toFortune) }
        .map { (it + Fortune(0, "Additional fortune added at request time.")) }
        .toCompletionStage().toCompletableFuture().get()
        .sortedBy { it.message }

    companion object {
        private fun findWorld(id: Int, pool: SqlClient) =
            pool.preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                .execute(Tuple.of(id))
                .map { toWorld(it.first()) }
    }
}

private fun toWorld(r: Row) = r.getInteger("id") to r.getInteger("randomnumber")

private fun toFortune(it: Row) = Fortune(it.getInteger(0), it.getString(1))
