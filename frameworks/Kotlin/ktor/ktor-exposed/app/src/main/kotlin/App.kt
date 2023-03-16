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
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.html.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import org.jetbrains.exposed.dao.IntEntity
import org.jetbrains.exposed.dao.IntEntityClass
import org.jetbrains.exposed.dao.id.EntityID
import org.jetbrains.exposed.dao.id.IdTable
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.SqlExpressionBuilder.eq
import org.jetbrains.exposed.sql.transactions.transaction
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
    embeddedServer(Netty, port = 8080) { module(exposedMode) }.start(wait = true)
}

fun Application.module(exposedMode: ExposedMode) {
    val dbRows = 10000
    val poolSize = 48
    val pool = HikariDataSource(HikariConfig().apply { configurePostgres(poolSize) })
    Database.connect(pool)
    suspend fun <T> withDatabaseContextAndTransaction(statement: Transaction.() -> T) =
        withContext(Dispatchers.IO) { transaction(statement = statement) }

    install(DefaultHeaders)

    routing {
        fun selectWorldsWithIdQuery(id: Int) =
            WorldTable.slice(WorldTable.id, WorldTable.randomNumber).select(WorldTable.id eq id)

        fun ResultRow.toWorld() =
            World(this[WorldTable.id].value, this[WorldTable.randomNumber])

        fun ResultRow.toFortune() =
            Fortune(this[FortuneTable.id].value, this[FortuneTable.message])

        fun ThreadLocalRandom.nextIntWithinRows() =
            nextInt(dbRows) + 1

        fun selectSingleWorld(random: ThreadLocalRandom): World =
            selectWorldsWithIdQuery(random.nextIntWithinRows()).single().toWorld()

        fun selectWorlds(queries: Int, random: ThreadLocalRandom): List<World> =
            List(queries) { selectSingleWorld(random) }

        get("/db") {
            val random = ThreadLocalRandom.current()
            val result = withDatabaseContextAndTransaction {
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

            val result = withDatabaseContextAndTransaction {
                when (exposedMode) {
                    Dsl -> selectWorlds(queries, random)
                    Dao -> //List(queries) { WorldDao[random.nextIntWithinRows()].toWorld() }
                        throw IllegalArgumentException("DAO not supported because it appears to cache results")
                }
            }

            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
        }

        get("/fortunes") {
            val result = withDatabaseContextAndTransaction {
                when (exposedMode) {
                    Dsl -> FortuneTable.slice(FortuneTable.id, FortuneTable.message).selectAll()
                        .asSequence().map { it.toFortune() }

                    Dao -> FortuneDao.all().asSequence().map { it.toFortune() }
                }.toMutableList()
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

            withDatabaseContextAndTransaction {
                when (exposedMode) {
                    Dsl -> {
                        result = selectWorlds(queries, random)
                        result.forEach { it.randomNumber = random.nextInt(dbRows) + 1 }
                        result
                            // sort the data to avoid data race because all updates are in one transaction
                            .sortedBy { it.id }
                            .forEach { world ->
                                WorldTable.update({ WorldTable.id eq world.id }) {
                                    it[randomNumber] = world.randomNumber
                                }
                                /*
                                // An alternative approach: commit every change to avoid data race
                                commit()
                                */
                            }
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
