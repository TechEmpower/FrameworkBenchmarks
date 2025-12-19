import database.*
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.single
import kotlinx.coroutines.flow.toList
import org.jetbrains.exposed.v1.core.dao.id.EntityID
import org.jetbrains.exposed.v1.core.statements.BatchUpdateStatement
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabase
import org.jetbrains.exposed.v1.r2dbc.select
import org.jetbrains.exposed.v1.r2dbc.statements.toExecutable
import org.jetbrains.exposed.v1.r2dbc.transactions.TransactionManager
import org.jetbrains.exposed.v1.r2dbc.transactions.suspendTransaction

// copied and adapted from https://github.com/huanshankeji/FrameworkBenchmarks/blob/34532d12439d95c939bde1044a5f11afd07927d1/frameworks/Kotlin/ktor/ktor-exposed/app/src/main/kotlin/App.kt#L148-L185
class MainVerticle : CommonWithDbVerticle<R2dbcDatabase>() {
    override suspend fun initDbClient(): R2dbcDatabase =
        r2DbcDatabaseConnect()

    override val httpServerStrictThreadMode get() = false
    //override val coHandlerCoroutineContext: CoroutineContext get() = EmptyCoroutineContext

    override suspend fun selectWorld(id: Int): World =
        suspendTransaction(dbClient) {
            r2dbcSelectWorldWithIdQuery(id).single().toWorld()
        }

    override suspend fun updateSortedWorlds(sortedWorlds: List<World>) {
        suspendTransaction(dbClient) {
            val batch = BatchUpdateStatement(WorldTable)
            sortedWorlds.forEach { world ->
                batch.addBatch(EntityID(world.id, WorldTable))
                batch[WorldTable.randomNumber] = world.randomNumber
            }
            // TODO also consider passing the transaction explicitly
            batch.toExecutable().execute(TransactionManager.current())
        }
    }

    override suspend fun selectFortunesInto(fortunes: MutableList<Fortune>) {
        suspendTransaction(dbClient) {
            FortuneTable.select(FortuneTable.id, FortuneTable.message)
                .map { it.toFortune() }.toList(fortunes)
        }
    }
}