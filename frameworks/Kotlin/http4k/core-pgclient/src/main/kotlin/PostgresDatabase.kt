import io.vertx.core.CompositeFuture
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
        queryPool.findWorld(random.world()).toCompletionStage().toCompletableFuture().get()

    override fun loadAll() = queryPool.preparedQuery("SELECT id, randomnumber FROM world ")
        .execute()
        .map { it.map(::toWorld) }
        .toCompletionStage().toCompletableFuture().get()

    override fun findWorlds(count: Int) =
        Future
            .all(
                (1..count).map { queryPool.findWorld(random.world()) }
            ).toCompletionStage().toCompletableFuture().get().list<World>()

    override fun updateWorlds(count: Int) =
        Future.all((1..count)
            .map { queryPool.findWorld(random.world()) })
            .map<List<World>>(CompositeFuture::list)
            .toCompletionStage()
            .thenCompose { worlds ->
                updatePool.preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
                    .executeBatch((1..count).map { Tuple.of(random.world(), random.world()) })
                    .toCompletionStage()
                    .thenApply { worlds }
            }.toCompletableFuture().get()

    override fun fortunes() = queryPool.preparedQuery("SELECT id, message FROM fortune")
        .execute()
        .map { it.map(::toFortune) }
        .map { (it + Fortune(0, "Additional fortune added at request time.")) }
        .map { it.sortedBy { it.message } }
        .toCompletionStage().toCompletableFuture().get()

    companion object {
        private fun SqlClient.findWorld(id: Int) =
            preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                .execute(Tuple.of(id))
                .map { toWorld(it.single()) }
    }
}

private fun toWorld(r: Row) = r.getInteger("id") to r.getInteger("randomnumber")

private fun toFortune(it: Row) = Fortune(it.getInteger(0), it.getString(1))
