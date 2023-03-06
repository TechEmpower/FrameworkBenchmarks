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
import org.jetbrains.exposed.dao.id.IdTable
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.SqlExpressionBuilder.eq
import org.jetbrains.exposed.sql.transactions.transaction
import java.util.concurrent.ThreadLocalRandom

@Serializable
data class Message(val message: String)

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


fun main() {
    embeddedServer(Netty, port = 8080, module = Application::module).start(wait = true)
}

fun Application.module() {
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

        fun Transaction.selectSingleWorld(random: ThreadLocalRandom): World {
            val id = random.nextInt(dbRows) + 1
            return selectWorldsWithIdQuery(id).single().toWorld()
        }


        get("/db") {
            val random = ThreadLocalRandom.current()
            val world = withDatabaseContextAndTransaction { selectSingleWorld(random) }
            call.respondText(Json.encodeToString(world), ContentType.Application.Json)
        }

        fun Transaction.selectWorlds(queries: Int, random: ThreadLocalRandom): List<World> =
            List(queries) { selectSingleWorld(random) }

        get("/queries") {
            val queries = call.queries()
            val random = ThreadLocalRandom.current()

            val result = withDatabaseContextAndTransaction { selectWorlds(queries, random) }

            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
        }

        get("/fortunes") {
            lateinit var result: MutableList<Fortune>

            withDatabaseContextAndTransaction {
                val query = FortuneTable.slice(FortuneTable.id, FortuneTable.message).selectAll()
                result = query.asSequence().map { it.toFortune() }.toMutableList()
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
                result = selectWorlds(queries, random)
                result.forEach { it.randomNumber = random.nextInt(dbRows) + 1 }
                result.forEach { world ->
                    WorldTable.update({ WorldTable.id eq world.id }) { it[randomNumber] = world.randomNumber }
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
