import database.*
import io.r2dbc.spi.Connection
import io.r2dbc.spi.Readable
import kotlinx.coroutines.reactive.awaitSingle
import kotlinx.coroutines.reactive.collect

// TODO also try getting new connections each time
class MainVerticle : CommonWithDbVerticle<Connection>() {
    override suspend fun initDbClient(): Connection =
        connectionFactory.create().awaitSingle()

    override val httpServerStrictThreadMode get() = false
    //override val coHandlerCoroutineContext: CoroutineContext get() = EmptyCoroutineContext

    override suspend fun selectWorld(id: Int): World =
        dbClient.createStatement(SELECT_WORLD_SQL).bind(0, id).execute()
            .awaitSingle()
            .map(Readable::toWorld)
            .awaitSingle()

    override suspend fun updateSortedWorlds(sortedWorlds: List<World>) {
        val statement = dbClient.createStatement(UPDATE_WORLD_SQL)
        val lastIndex = sortedWorlds.lastIndex
        sortedWorlds.forEachIndexed { index, world ->
            statement.bind(0, world.randomNumber)
                .bind(1, world.id)
            if (index < lastIndex) statement.add()
        }
        // wait for the execution to complete
        statement.execute().awaitSingle() // or `.collect {}`
    }

    override suspend fun selectFortunesInto(fortunes: MutableList<Fortune>) {
        dbClient.createStatement(SELECT_FORTUNE_SQL).execute()
            .awaitSingle()
            .map(Readable::toFortune)
            //.asFlow().toList(fortunes)
            .collect(fortunes::add)
    }
}