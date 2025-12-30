import io.vertx.core.Future
import io.vertx.pgclient.PgConnectOptions
import io.vertx.pgclient.PgPool.client
import io.vertx.sqlclient.PoolOptions
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.SqlClient
import io.vertx.sqlclient.Tuple
import java.util.Random


class PostgresDatabase : Database {
    private val dbPool = run {
        val connectOptions = PgConnectOptions().apply {
            port = 5432
            cachePreparedStatements = true
            host = "tfb-database"
            database = "hello_world"
            user = "benchmarkdbuser"
            password = "benchmarkdbpass"
        }
        client(connectOptions, PoolOptions().apply { maxSize = 64 })
    }

    private val random = Random()

    override fun findWorld() = dbPool.findWorld(random.world())

    override fun loadAll() = dbPool.preparedQuery("SELECT id, randomnumber FROM world")
        .execute()
        .map { it.map(::toWorld) }
        .awaitComplete()

    override fun findWorlds(count: Int) = (1..count)
        .map { dbPool.findWorld(random.world()) }

    override fun updateWorlds(count: Int) = (1..count)
        .map { World(random.world(), random.world()) }
        .onEach {
            dbPool
                .preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                .execute(Tuple.of(it.first))
                .map { rowSet ->
                    val row = rowSet.iterator().next()
                    row.getInteger(1)
                    dbPool
                        .preparedQuery("UPDATE world SET randomnumber = $1 WHERE id = $2")
                        .execute(Tuple.of(it.second, it.first))
                }
                .awaitComplete()
        }

    override fun fortunes() = dbPool.preparedQuery("SELECT id, message FROM fortune")
        .execute()
        .map { it.map { Fortune(it.getInteger(0), it.getString(1)) } }
        .map { (it + Fortune(0, "Additional fortune added at request time.")) }
        .map { it.sortedBy { it.message } }
        .awaitComplete()

    companion object {
        private fun SqlClient.findWorld(id: Int) =
            preparedQuery("SELECT id, randomnumber FROM world WHERE id = $1")
                .execute(Tuple.of(id))
                .map { it.map(::toWorld).first() }
                .awaitComplete()
    }
}

private fun toWorld(r: Row) = r.getInteger("id") to r.getInteger("randomnumber")

private fun <T> Future<T>.awaitComplete(): T = toCompletionStage().toCompletableFuture().get()