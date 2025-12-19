import database.*
import io.vertx.kotlin.coroutines.coAwait
import io.vertx.kotlin.pgclient.pgConnectOptionsOf
import io.vertx.pgclient.PgConnection
import io.vertx.sqlclient.PreparedQuery
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.RowSet
import io.vertx.sqlclient.Tuple

// `PgConnection`s as used in the "vertx" portion offers better performance than `PgPool`s.
class MainVerticle : CommonWithDbVerticle<PgConnection>() {
    lateinit var selectWorldQuery: PreparedQuery<RowSet<Row>>
    lateinit var selectFortuneQuery: PreparedQuery<RowSet<Row>>
    lateinit var updateWorldQuery: PreparedQuery<RowSet<Row>>

    override suspend fun initDbClient(): PgConnection =
        // Parameters are copied from the "vertx-web" and "vertx" portions.
        PgConnection.connect(
            vertx,
            pgConnectOptionsOf(
                database = "hello_world",
                host = "tfb-database",
                user = "benchmarkdbuser",
                password = "benchmarkdbpass",
                cachePreparedStatements = true,
                pipeliningLimit = 256
            )
        ).coAwait().apply {
            selectWorldQuery = preparedQuery(SELECT_WORLD_SQL)
            selectFortuneQuery = preparedQuery(SELECT_FORTUNE_SQL)
            updateWorldQuery = preparedQuery(UPDATE_WORLD_SQL)
        }


    override suspend fun selectWorld(id: Int) =
        selectWorldQuery.execute(Tuple.of(id)).coAwait()
            .single().toWorld()

    override suspend fun selectFortunesInto(fortunes: MutableList<Fortune>) {
        selectFortuneQuery.execute().coAwait()
            .mapTo(fortunes) { it.toFortune() }
    }

    override suspend fun updateSortedWorlds(sortedWorlds: List<World>) {
        updateWorldQuery.executeBatch(sortedWorlds.map { Tuple.of(it.randomNumber, it.id) }).coAwait()
    }
}