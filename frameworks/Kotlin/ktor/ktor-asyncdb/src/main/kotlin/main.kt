import com.github.jasync.sql.db.ConnectionPoolConfiguration
import com.github.jasync.sql.db.QueryResult
import com.github.jasync.sql.db.SuspendingConnection
import com.github.jasync.sql.db.asSuspending
import com.github.jasync.sql.db.postgresql.PostgreSQLConnectionBuilder
import io.ktor.http.ContentType
import io.ktor.server.application.*
import io.ktor.server.engine.embeddedServer
import io.ktor.server.html.*
import io.ktor.server.netty.Netty
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.coroutines.*
import kotlinx.html.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import java.lang.IllegalArgumentException
import kotlin.random.Random
import kotlin.random.nextInt

@Serializable
data class Message(val message: String)

@Serializable
data class World(val id: Int, val randomNumber: Int)

data class Fortune(val id: Int, val message: String)

val rand = Random(1)

interface Repository {
    suspend fun getWorld(): World
    suspend fun getFortunes(): List<Fortune>
    suspend fun updateWorlds(worlds: List<World>)
}

class JasyncRepository() : Repository {
    companion object {
        const val WORLD_QUERY = "select id, randomNumber from world where id = ?"
        const val FORTUNES_QUERY = "select id, message from fortune"
        const val UPDATE_QUERY = "update world set randomNumber = ? where id = ?"
    }

    private val dbConfig: ConnectionPoolConfiguration = ConnectionPoolConfiguration(
        "tfb-database",
        database = "hello_world",
        username = "benchmarkdbuser",
        password = "benchmarkdbpass",
        maxActiveConnections = 64
    )
    private val db: SuspendingConnection = PostgreSQLConnectionBuilder.createConnectionPool(dbConfig).asSuspending

    override suspend fun getWorld(): World {
        val worldId = rand.nextInt(1, 10000)
        val result = db.sendPreparedStatement(WORLD_QUERY, listOf(worldId))
        val row = result.rows.first()
        return World(row.getInt(0)!!, row.getInt(1)!!)
    }

    override suspend fun getFortunes(): List<Fortune> {
        val results = db.sendPreparedStatement(FORTUNES_QUERY)
        return results.rows.map { Fortune(it.getInt(0)!!, it.getString(1)!!) }
    }

    override suspend fun updateWorlds(worlds: List<World>) {
        coroutineScope {
            val jobs = ArrayList<Deferred<QueryResult>>(worlds.size)
            worlds.forEach { world ->
                val deferred = async(Dispatchers.IO) {
                    db.sendPreparedStatement(
                        UPDATE_QUERY,
                        listOf(world.randomNumber, world.id)
                    )
                }
                jobs.add(deferred)
            }

            jobs.awaitAll()
        }
    }
}

fun String.toBoxedInt(range: IntRange): Int = try {
    this.toInt().coerceIn(range)
} catch (e: NumberFormatException) {
    1
}

class MainTemplate : Template<HTML> {
    val content = Placeholder<HtmlBlockTag>()
    override fun HTML.apply() {
        head {
            title { +"Fortunes" }
        }
        body {
            insert(content)
        }
    }
}

class FortuneTemplate(private val fortunes: List<Fortune>, private val main: MainTemplate = MainTemplate()) : Template<HTML> {
    override fun HTML.apply() {
        insert(main) {
            content {
                table {
                    tr {
                        th { +"id" }
                        th { +"message" }
                    }
                    fortunes.forEach { fortune ->
                        tr {
                            td { +fortune.id.toString() }
                            td { +fortune.message }
                        }
                    }
                }
            }
        }
    }
}

fun main(args: Array<String>) {
    val db = when(args.firstOrNull()) {
        "jasync-sql" -> JasyncRepository()
        else -> throw IllegalArgumentException("Must specify a postgres client")
    }

    val server = embeddedServer(Netty, 8080, configure = {
        shareWorkGroup = true
    }) {
        install(DefaultHeaders)
        routing {
            get("/plaintext") {
                call.respondText("Hello, World!")
            }

            get("/json") {
                call.respondText(
                    Json.encodeToString(Message("Hello, World!")),
                    ContentType.Application.Json
                )
            }

            get("/db") {
                call.respondText(Json.encodeToString(db.getWorld()), ContentType.Application.Json)
            }

            get("/query/") {
                val queries = call.parameters["queries"]?.toBoxedInt(1..500) ?: 1
                val worlds = (1..queries).map { db.getWorld() }
                call.respondText(Json.encodeToString(worlds), ContentType.Application.Json)
            }

            get("/fortunes") {
                val newFortune = Fortune(0, "Additional fortune added at request time.")
                val fortunes = db.getFortunes().toMutableList()
                fortunes.add(newFortune)
                fortunes.sortBy { it.message }
                call.respondHtmlTemplate(FortuneTemplate(fortunes)) { }
            }

            get("/updates") {
                val queries = call.parameters["queries"]?.toBoxedInt(1..500) ?: 1
                val worlds = (1..queries).map { db.getWorld() }
                val newWorlds = worlds.map { it.copy(randomNumber = rand.nextInt(1..10000)) }

                db.updateWorlds(newWorlds)

                call.respondText(Json.encodeToString(newWorlds), ContentType.Application.Json)
            }
        }
    }

    server.start(wait = true)
}
