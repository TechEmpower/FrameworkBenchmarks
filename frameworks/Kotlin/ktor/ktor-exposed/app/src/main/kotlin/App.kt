import ExposedMode.Dao
import ExposedMode.Dsl
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import database.*
import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.html.*
import io.ktor.server.netty.*
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.single
import kotlinx.coroutines.flow.toList
import kotlinx.html.*
import org.jetbrains.exposed.v1.core.dao.id.EntityID
import org.jetbrains.exposed.v1.core.eq
import org.jetbrains.exposed.v1.core.statements.BatchUpdateStatement
import org.jetbrains.exposed.v1.core.vendors.PostgreSQLDialect
import org.jetbrains.exposed.v1.jdbc.Database
import org.jetbrains.exposed.v1.jdbc.transactions.suspendTransaction
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabase
import org.jetbrains.exposed.v1.r2dbc.R2dbcDatabaseConfig
import org.jetbrains.exposed.v1.r2dbc.transactions.suspendTransaction
import java.util.concurrent.ThreadLocalRandom
import org.jetbrains.exposed.v1.jdbc.select as jdbcSelect
import org.jetbrains.exposed.v1.jdbc.statements.toExecutable as toJdbcExecutable
import org.jetbrains.exposed.v1.jdbc.transactions.TransactionManager.Companion as JdbcTransactionManager
import org.jetbrains.exposed.v1.r2dbc.select as r2dbcSelect
import org.jetbrains.exposed.v1.r2dbc.statements.toExecutable as toR2dbcExecutable
import org.jetbrains.exposed.v1.r2dbc.transactions.TransactionManager as R2dbcTransactionManager

enum class ConnectionMode {
    Jdbc, R2dbc
}

enum class ExposedMode {
    Dsl, Dao
}

fun main(args: Array<String>) {
    val connectionMode = ConnectionMode.valueOf(args[0])
    val exposedMode = ExposedMode.valueOf(args[1])
    embeddedServer(Netty, port = 9090) { module(connectionMode, exposedMode) }.start(wait = true)
}


fun Application.module(connectionMode: ConnectionMode, exposedMode: ExposedMode) =
    parameterizedModule(
        when (connectionMode) {
            ConnectionMode.Jdbc -> when (exposedMode) {
                Dsl -> ExposedOps.Jdbc.Dsl
                Dao -> ExposedOps.Jdbc.Dao
            }

            ConnectionMode.R2dbc -> when (exposedMode) {
                Dsl -> ExposedOps.R2dbc.Dsl
                Dao -> throw IllegalArgumentException("DAO with R2DBC is not supported")
            }
        }
    )

fun ApplicationCall.queries() =
    request.queryParameters["queries"]?.toIntOrNull()?.coerceIn(1, 500) ?: 1

private const val DB_ROWS = 10_000
fun ThreadLocalRandom.nextIntWithinRows() =
    nextInt(DB_ROWS) + 1

interface ExposedOps<TDatabase> {
    fun createDatabase(): TDatabase
    suspend fun <T> transaction(db: TDatabase, statement: suspend /*JdbcTransaction.*/() -> T): T

    // Repository pattern functions. These can also be extracted into a separate interface.
    suspend fun getWorldWithId(id: Int): World
    suspend fun getRandomWorlds(queries: Int, random: ThreadLocalRandom): List<World>
    suspend fun getAllFortunesAndAddTo(result: MutableList<Fortune>)
    suspend fun getRandomWorldsAndUpdate(queries: Int, random: ThreadLocalRandom): List<World>

    interface Jdbc : ExposedOps<Database> {
        override fun createDatabase(): Database {
            val poolSize = Runtime.getRuntime().availableProcessors() * 2
            val pool = HikariDataSource(HikariConfig().apply { configurePostgres(poolSize) })
            return Database.connect(pool)
        }

        override suspend fun <T> transaction(db: Database, statement: suspend () -> T): T =
            suspendTransaction(db) { statement() }

        object Dsl : Jdbc {
            override suspend fun getWorldWithId(id: Int): World =
                WorldTable.jdbcSelect(WorldTable.id, WorldTable.randomNumber).where(WorldTable.id eq id)
                    .single().toWorld()

            override suspend fun getRandomWorlds(queries: Int, random: ThreadLocalRandom): List<World> =
                List(queries) { getWorldWithId(random.nextIntWithinRows()) }

            override suspend fun getAllFortunesAndAddTo(result: MutableList<Fortune>) {
                FortuneTable.jdbcSelect(FortuneTable.id, FortuneTable.message)
                    .mapTo(result) { it.toFortune() }
            }

            override suspend fun getRandomWorldsAndUpdate(queries: Int, random: ThreadLocalRandom): List<World> {
                val result = getRandomWorlds(queries, random)
                result.forEach { it.randomNumber = random.nextIntWithinRows() }
                val batch = BatchUpdateStatement(WorldTable)
                result.sortedBy { it.id }.forEach { world ->
                    batch.addBatch(EntityID(world.id, WorldTable))
                    batch[WorldTable.randomNumber] = world.randomNumber
                }
                // also consider passing the transaction explicitly
                batch.toJdbcExecutable().execute(JdbcTransactionManager.current())
                return result
            }
        }

        object Dao : Jdbc {
            override suspend fun getWorldWithId(id: Int): World =
                WorldDao[id].toWorld()

            override suspend fun getRandomWorlds(queries: Int, random: ThreadLocalRandom): List<World> =
                //List(queries) { WorldDao[random.nextIntWithinRows()].toWorld() }
                throw IllegalArgumentException("DAO not supported because it appears to cache results")

            override suspend fun getAllFortunesAndAddTo(result: MutableList<Fortune>) {
                FortuneDao.all().mapTo(result) { it.toFortune() }
            }

            override suspend fun getRandomWorldsAndUpdate(queries: Int, random: ThreadLocalRandom): List<World> {
                /*
                val worldDaosAndNewRandomNumbers =
                    List(queries) { WorldDao[random.nextIntWithinRows()] to random.nextIntWithinRows() }
                worldDaosAndNewRandomNumbers
                    .sortedBy { (worldDao, _) -> worldDao.id.value }
                    .forEach { (worldDao, newRandomNumber) ->
                        worldDao.randomNumber = newRandomNumber
                    }
                val result = worldDaosAndNewRandomNumbers.map { (worldDao, _) -> worldDao.toWorld() }
                */
                throw IllegalArgumentException("DAO not supported because it appears to cache results")
            }
        }
    }

    // TODO consider moving to separate files to avoid import conflicts/aliases
    interface R2dbc : ExposedOps<R2dbcDatabase> {
        override fun createDatabase(): R2dbcDatabase =
            R2dbcDatabase.connect(configurePostgresR2DBC(), R2dbcDatabaseConfig {
                // This can't be omitted.
                explicitDialect = PostgreSQLDialect()
            })

        override suspend fun <T> transaction(db: R2dbcDatabase, statement: suspend () -> T): T =
            suspendTransaction(db) { statement() }

        object Dsl : R2dbc {
            override suspend fun getWorldWithId(id: Int): World =
                WorldTable.r2dbcSelect(WorldTable.id, WorldTable.randomNumber).where(WorldTable.id eq id)
                    .single().toWorld()

            override suspend fun getRandomWorlds(queries: Int, random: ThreadLocalRandom): List<World> =
                List(queries) { getWorldWithId(random.nextIntWithinRows()) }

            override suspend fun getAllFortunesAndAddTo(result: MutableList<Fortune>) {
                FortuneTable.r2dbcSelect(FortuneTable.id, FortuneTable.message)
                    .map { it.toFortune() }.toList(result)
            }

            override suspend fun getRandomWorldsAndUpdate(queries: Int, random: ThreadLocalRandom): List<World> {
                val result = getRandomWorlds(queries, random)
                result.forEach { it.randomNumber = random.nextIntWithinRows() }
                val batch = BatchUpdateStatement(WorldTable)
                result.sortedBy { it.id }.forEach { world ->
                    batch.addBatch(EntityID(world.id, WorldTable))
                    batch[WorldTable.randomNumber] = world.randomNumber
                }
                // also consider passing the transaction explicitly
                batch.toR2dbcExecutable().execute(R2dbcTransactionManager.current())
                return result
            }
        }
    }
}

fun <Database> Application.parameterizedModule(exposedOps: ExposedOps<Database>) {
    install(DefaultHeaders)

    routing {
        val database = exposedOps.createDatabase()

        suspend fun <T> transaction(statement: suspend () -> T): T =
            exposedOps.transaction(database) { statement() }

        get("/db") {
            val random = ThreadLocalRandom.current()
            val result = transaction { exposedOps.getWorldWithId(random.nextIntWithinRows()) }
            call.respondText(json.encodeToString(result), ContentType.Application.Json)
        }

        get("/queries") {
            val queries = call.queries()
            val random = ThreadLocalRandom.current()
            val result = transaction { exposedOps.getRandomWorlds(queries, random) }
            call.respondText(json.encodeToString(result), ContentType.Application.Json)
        }

        get("/fortunes") {
            val result = mutableListOf<Fortune>()

            transaction { exposedOps.getAllFortunesAndAddTo(result) }

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
            val result = transaction { exposedOps.getRandomWorldsAndUpdate(queries, random) }
            call.respondText(json.encodeToString(result), ContentType.Application.Json)
        }
    }
}
