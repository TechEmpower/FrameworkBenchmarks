import ExposedMode.*
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.html.*
import io.ktor.server.netty.*
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.html.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import org.jetbrains.exposed.v1.core.ResultRow
import org.jetbrains.exposed.v1.core.Transaction
import org.jetbrains.exposed.v1.core.dao.id.EntityID
import org.jetbrains.exposed.v1.core.dao.id.IdTable
import org.jetbrains.exposed.v1.core.eq
import org.jetbrains.exposed.v1.core.statements.BatchUpdateStatement
import org.jetbrains.exposed.v1.dao.IntEntity
import org.jetbrains.exposed.v1.dao.IntEntityClass
import org.jetbrains.exposed.v1.jdbc.Database
import org.jetbrains.exposed.v1.jdbc.select
import org.jetbrains.exposed.v1.jdbc.statements.toExecutable
import org.jetbrains.exposed.v1.jdbc.transactions.TransactionManager
import org.jetbrains.exposed.v1.jdbc.transactions.suspendTransaction
import java.util.concurrent.ThreadLocalRandom

@Serializable
data class World(val id: Int, var randomNumber: Int)

@Serializable
data class Fortune(val id: Int, var message: String)


// see "toolset/databases/postgres/create-postgres.sql"

object WorldTable : IdTable<Int>("World") {
    override val id = integer("id").entityId()
    val randomNumber = integer("randomnumber").default(0) // The name is "randomNumber" in "create-postgres.sql".
}

object FortuneTable : IdTable<Int>("Fortune") {
    override val id = integer("id").entityId()
    val message = varchar("message", 2048)
}


class WorldDao(id: EntityID<Int>) : IntEntity(id) {
    companion object : IntEntityClass<WorldDao>(WorldTable)

    var randomNumber by WorldTable.randomNumber
    fun toWorld() =
        World(id.value, randomNumber)
}

class FortuneDao(id: EntityID<Int>) : IntEntity(id) {
    companion object : IntEntityClass<FortuneDao>(FortuneTable)

    var message by FortuneTable.message
    fun toFortune() =
        Fortune(id.value, message)
}


enum class ExposedMode {
    Dsl, Dao
}

fun main(args: Array<String>) {
    val exposedMode = valueOf(args.first())
    embeddedServer(Netty, port = 9090) { module(exposedMode) }.start(wait = true)
}

fun Application.module(exposedMode: ExposedMode) {
    val poolSize = Runtime.getRuntime().availableProcessors() * 2
    val pool = HikariDataSource(HikariConfig().apply { configurePostgres(poolSize) })
    val database = Database.connect(pool)
    suspend fun <T> withDatabaseTransaction(statement: suspend Transaction.() -> T) =
        suspendTransaction(database, statement = statement)

    install(DefaultHeaders)

    routing {
        fun selectWorldsWithIdQuery(id: Int) =
            WorldTable.select(WorldTable.id, WorldTable.randomNumber).where(WorldTable.id eq id)

        fun ResultRow.toWorld() =
            World(this[WorldTable.id].value, this[WorldTable.randomNumber])

        fun ResultRow.toFortune() =
            Fortune(this[FortuneTable.id].value, this[FortuneTable.message])

        fun ThreadLocalRandom.nextIntWithinRows() =
            nextInt(DB_ROWS) + 1

        fun selectSingleWorld(random: ThreadLocalRandom): World =
            selectWorldsWithIdQuery(random.nextIntWithinRows()).single().toWorld()

        fun selectWorlds(queries: Int, random: ThreadLocalRandom): List<World> =
            List(queries) { selectSingleWorld(random) }

        get("/db") {
            val random = ThreadLocalRandom.current()
            val result = withDatabaseTransaction<World> {
                when (exposedMode) {
                    Dsl -> selectSingleWorld(random)
                    Dao -> WorldDao[random.nextIntWithinRows()].toWorld()
                }
            }
            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
        }


        get("/queries") {
            val queries = call.queries()
            val random = ThreadLocalRandom.current()

            val result = withDatabaseTransaction<List<World>> {
                when (exposedMode) {
                    Dsl -> selectWorlds(queries, random)
                    Dao -> //List(queries) { WorldDao[random.nextIntWithinRows()].toWorld() }
                        throw IllegalArgumentException("DAO not supported because it appears to cache results")
                }
            }

            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
        }

        get("/fortunes") {
            val result = mutableListOf<Fortune>()
            withDatabaseTransaction {
                when (exposedMode) {
                    Dsl -> FortuneTable.select(FortuneTable.id, FortuneTable.message).toList()
                        .mapTo(result) { it.toFortune() }

                    Dao -> FortuneDao.all().mapTo(result) { it.toFortune() }
                }
            }

            result.add(Fortune(0, "Additional fortune added at request time."))
            result.sortBy { it.message }
            call.respondHtml {
                head { title { +"Fortunes" } }
                body {
                    table {
                        tr {
                            th { +"id" }
                            th { +"message" }
                        }
                        for (fortune in result) {
                            tr {
                                td { +fortune.id.toString() }
                                td { +fortune.message }
                            }
                        }
                    }
                }
            }
        }

        get("/updates") {
            val queries = call.queries()
            val random = ThreadLocalRandom.current()
            lateinit var result: List<World>

            withDatabaseTransaction {
                when (exposedMode) {
                    Dsl -> {
                        result = selectWorlds(queries, random)
                        result.forEach { it.randomNumber = random.nextIntWithinRows() }
                        val batch = BatchUpdateStatement(WorldTable)
                        result.sortedBy { it.id }.forEach { world ->
                            batch.addBatch(EntityID(world.id, WorldTable))
                            batch[WorldTable.randomNumber] = world.randomNumber
                        }
                        batch.toExecutable().execute(TransactionManager.current())
                    }

                    Dao -> /*{
                        val worldDaosAndNewRandomNumbers =
                            List(queries) { WorldDao[random.nextIntWithinRows()] to random.nextIntWithinRows() }
                        worldDaosAndNewRandomNumbers
                            .sortedBy { (worldDao, _) -> worldDao.id.value }
                            .forEach { (worldDao, newRandomNumber) ->
                                worldDao.randomNumber = newRandomNumber
                            }
                        result = worldDaosAndNewRandomNumbers.map { (worldDao, _) -> worldDao.toWorld() }
                    }*/
                        throw IllegalArgumentException("DAO not supported because it appears to cache results")
                }
            }

            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
        }
    }
}

private const val DB_ROWS = 10_000

fun HikariConfig.configurePostgres(poolSize: Int) {
    jdbcUrl = "jdbc:postgresql://tfb-database/hello_world?useSSL=false"
    driverClassName = org.postgresql.Driver::class.java.name

    configureCommon(poolSize)
}

fun HikariConfig.configureCommon(poolSize: Int) {
    username = "benchmarkdbuser"
    password = "benchmarkdbpass"
    addDataSourceProperty("cacheServerConfiguration", true)
    addDataSourceProperty("cachePrepStmts", "true")
    addDataSourceProperty("useUnbufferedInput", "false")
    addDataSourceProperty("prepStmtCacheSize", "4096")
    addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
    connectionTimeout = 10000
    maximumPoolSize = poolSize
    minimumIdle = poolSize
}

fun ApplicationCall.queries() =
    request.queryParameters["queries"]?.toIntOrNull()?.coerceIn(1, 500) ?: 1

