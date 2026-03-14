import com.huanshankeji.exposedvertxsqlclient.DatabaseClient
import com.huanshankeji.exposedvertxsqlclient.ExperimentalEvscApi
import com.huanshankeji.exposedvertxsqlclient.JdbcTransactionExposedTransactionProvider
import com.huanshankeji.exposedvertxsqlclient.postgresql.PgDatabaseClientConfig
import com.huanshankeji.exposedvertxsqlclient.postgresql.vertx.pgclient.createPgConnection
import database.*
import io.vertx.pgclient.PgConnection
import org.jetbrains.exposed.v1.core.eq
import org.jetbrains.exposed.v1.core.statements.buildStatement
import org.jetbrains.exposed.v1.jdbc.Database
import org.jetbrains.exposed.v1.jdbc.select

@OptIn(ExperimentalEvscApi::class)
class MainVerticle(val exposedDatabase: Database) : CommonWithDbVerticle<DatabaseClient<PgConnection>, Unit>(),
    CommonWithDbVerticleI.ParallelOrPipelinedSelectWorlds<DatabaseClient<PgConnection>, Unit>,
    CommonWithDbVerticleI.WithoutTransaction<DatabaseClient<PgConnection>> {
    // kept in case we support generating and reusing `PreparedQuery`
    /*
    lateinit var selectWorldQuery: PreparedQuery<RowSet<Row>>
    lateinit var selectFortuneQuery: PreparedQuery<RowSet<Row>>
    lateinit var updateWorldQuery: PreparedQuery<RowSet<Row>>
    */

    override suspend fun initDbClient(): DatabaseClient<PgConnection> {
        // Parameters are copied from the "vertx-web" and "vertx" portions.
        val pgConnection = createPgConnection(vertx, connectionConfig, {
            cachePreparedStatements = true
            pipeliningLimit = 256
        })
        return DatabaseClient(
            pgConnection,
            PgDatabaseClientConfig(JdbcTransactionExposedTransactionProvider(exposedDatabase), validateBatch = false)
        )
    }

    override suspend fun Unit.selectWorld(id: Int): World =
        dbClient.executeQuery(jdbcSelectWorldWithIdQuery(id))
            .single().toWorld()

    override suspend fun Unit.updateSortedWorlds(sortedWorlds: List<World>) {
        dbClient.executeBatchUpdate(sortedWorlds.map { world ->
            buildStatement {
                WorldTable.update({ WorldTable.id eq world.id }) {
                    it[randomNumber] = world.randomNumber
                }
            }
        })
    }

    override suspend fun Unit.selectFortunesInto(fortunes: MutableList<Fortune>) {
        dbClient.executeQuery(with(FortuneTable) { select(id, message) })
            .mapTo(fortunes) { it.toFortune() }
    }
}