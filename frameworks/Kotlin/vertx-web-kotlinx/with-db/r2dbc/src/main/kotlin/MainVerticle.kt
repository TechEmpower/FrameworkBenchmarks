import database.*
import io.r2dbc.spi.Connection
import io.r2dbc.spi.Readable
import kotlinx.coroutines.reactive.awaitFirst
import kotlinx.coroutines.reactive.awaitSingle
import kotlinx.coroutines.reactive.collect

/*
`ParallelOrPipelinedSelectWorlds` leads to `io.r2dbc.postgresql.client.ReactorNettyClient$RequestQueueException: [08006] Cannot exchange messages because the request queue limit is exceeded`.
https://github.com/pgjdbc/r2dbc-postgresql/issues/360#issuecomment-869422327 offers a workaround, but it doesn't seem like the officially recommended approach.
The PostgreSQL R2DBC driver doesn't seem to have full support for pipelining and multiplexing as discussed in https://github.com/pgjdbc/r2dbc-postgresql/pull/28.
 */
class MainVerticle : CommonWithDbVerticle<Connection, Unit>(),
    CommonWithDbVerticleI.SequentialSelectWorlds<Connection, Unit>,
    CommonWithDbVerticleI.WithoutTransaction<Connection> {
    override suspend fun initDbClient(): Connection =
        connectionFactory.create().awaitSingle()

    override suspend fun stop() {
        dbClient.close().awaitSingle()
    }

    override val httpServerStrictThreadMode get() = false
    //override val coHandlerCoroutineContext: CoroutineContext get() = EmptyCoroutineContext

    override suspend fun Unit.selectWorld(id: Int): World =
        dbClient.createStatement(SELECT_WORLD_SQL).bind(0, id).execute()
            .awaitSingle()
            .map(Readable::toWorld)
            .awaitSingle()

    override suspend fun Unit.updateSortedWorlds(sortedWorlds: List<World>) {
        val statement = dbClient.createStatement(UPDATE_WORLD_SQL)
        val lastIndex = sortedWorlds.lastIndex
        sortedWorlds.forEachIndexed { index, world ->
            statement.bind(0, world.randomNumber)
                .bind(1, world.id)
            if (index < lastIndex) statement.add()
        }
        // wait for the execution to complete
        // For batch statements, execute() returns multiple Result objects.
        // We must consume the rowsUpdated Publisher from each Result.
        statement.execute().collect { result ->
            result.rowsUpdated.awaitFirst()
        }
    }

    override suspend fun Unit.selectFortunesInto(fortunes: MutableList<Fortune>) {
        dbClient.createStatement(SELECT_FORTUNE_SQL).execute()
            .awaitSingle()
            .map(Readable::toFortune)
            //.asFlow().toList(fortunes)
            .collect(fortunes::add)
    }
}