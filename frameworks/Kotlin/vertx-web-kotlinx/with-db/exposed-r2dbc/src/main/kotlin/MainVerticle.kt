import database.*
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.single
import kotlinx.coroutines.flow.toList
import org.jetbrains.exposed.v1.core.dao.id.EntityID
import org.jetbrains.exposed.v1.core.statements.BatchUpdateStatement
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabase
import org.jetbrains.exposed.v1.r2dbc.R2dbcTransaction
import org.jetbrains.exposed.v1.r2dbc.select
import org.jetbrains.exposed.v1.r2dbc.statements.toExecutable
import org.jetbrains.exposed.v1.r2dbc.transactions.suspendTransaction

/*
`ParallelOrPipelinedSelectWorlds` results in `io.r2dbc.postgresql.client.ReactorNettyClient$RequestQueueException: [08006] Cannot exchange messages because the request queue limit is exceeded`.
https://github.com/pgjdbc/r2dbc-postgresql/issues/360#issuecomment-869422327 offers a workaround, but it doesn't seem like the officially recommended approach.
The PostgreSQL R2DBC driver doesn't seem to have full support for pipelining and multiplexing as discussed in https://github.com/pgjdbc/r2dbc-postgresql/pull/28.
 */
class MainVerticle(private val r2dbcDatabase: R2dbcDatabase) : CommonWithDbVerticle<R2dbcDatabase, R2dbcTransaction>(),
    CommonWithDbVerticleI.SequentialSelectWorlds<R2dbcDatabase, R2dbcTransaction> {
    override suspend fun initDbClient(): R2dbcDatabase =
        r2dbcDatabase

    override val httpServerStrictThreadMode get() = false
    //override val coHandlerCoroutineContext: CoroutineContext get() = EmptyCoroutineContext

    // copied and adapted from https://github.com/huanshankeji/FrameworkBenchmarks/blob/34532d12439d95c939bde1044a5f11afd07927d1/frameworks/Kotlin/ktor/ktor-exposed/app/src/main/kotlin/App.kt#L148-L185

    override suspend fun <T> withOptionalTransaction(block: suspend R2dbcTransaction.() -> T): T =
        suspendTransaction(dbClient) { block() }

    override suspend fun R2dbcTransaction.selectWorld(id: Int): World =
        r2dbcSelectWorldWithIdQuery(id).single().toWorld()

    override suspend fun R2dbcTransaction.updateSortedWorlds(sortedWorlds: List<World>) {
        val batch = BatchUpdateStatement(WorldTable)
        sortedWorlds.forEach { world ->
            batch.addBatch(EntityID(world.id, WorldTable))
            batch[WorldTable.randomNumber] = world.randomNumber
        }
        batch.toExecutable().execute(this)
    }

    override suspend fun R2dbcTransaction.selectFortunesInto(fortunes: MutableList<Fortune>) {
        FortuneTable.select(FortuneTable.id, FortuneTable.message)
            .map { it.toFortune() }.toList(fortunes)
    }
}

// Factory functions for creating R2dbcDatabase instances with different configurations

/**
 * Creates a MainVerticle that will create its own connection pool per verticle instance.
 * Used for separate-pool benchmark configurations.
 */
fun MainVerticleWithSeparatePool(poolSize: Int, useOptimizedConfig: Boolean): MainVerticle =
    MainVerticle(r2dbcConnectPool(poolSize, useOptimizedConfig))
