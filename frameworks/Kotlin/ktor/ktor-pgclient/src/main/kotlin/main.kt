import io.ktor.http.ContentType
import io.ktor.http.content.TextContent
import io.ktor.server.application.*
import io.ktor.server.html.*
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.vertx.kotlin.coroutines.coAwait
import io.vertx.pgclient.PgBuilder
import io.vertx.pgclient.PgConnectOptions
import io.vertx.sqlclient.PoolOptions
import io.vertx.sqlclient.Tuple
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.html.*
import java.util.concurrent.ThreadLocalRandom

val rand: ThreadLocalRandom
    get() = ThreadLocalRandom.current()

private const val HELLO_WORLD = "Hello, World!"
private const val WORLD_ROWS = 10_000

private fun nextWorldId(): Int = rand.nextInt(1, WORLD_ROWS + 1)

interface Repository {
    suspend fun getWorld(): World
    suspend fun getWorlds(count: Int): List<World>
    suspend fun getFortunes(): List<Fortune>
    suspend fun updateWorlds(worlds: List<World>)
}

class PgclientRepository : Repository {
    companion object {
        private const val FORTUNES_QUERY = "select id, message from FORTUNE"
        private const val SELECT_WORLD_QUERY = "SELECT id, randomnumber from WORLD where id=\$1"
        private const val UPDATE_WORLD_QUERY = "UPDATE WORLD SET randomnumber=\$1 WHERE id=\$2"
    }

    private val connectOptions =
        PgConnectOptions().apply {
            port = 5432
            host = "tfb-database"
            database = "hello_world"
            user = "benchmarkdbuser"
            password = "benchmarkdbpass"
            cachePreparedStatements = true
            pipeliningLimit = 100000
        }

    private val poolSize = Runtime.getRuntime().availableProcessors() * 2
    private val poolOptions = PoolOptions()
        .setMaxSize(poolSize)
        .setMaxWaitQueueSize(poolSize * 2)
    private val client = PgBuilder.client()
        .with(poolOptions)
        .connectingTo(connectOptions)
        .build()

    private val selectWorldStatement = client.preparedQuery(SELECT_WORLD_QUERY)
    private val updateWorldStatement = client.preparedQuery(UPDATE_WORLD_QUERY)
    private val fortunesStatement = client.preparedQuery(FORTUNES_QUERY)

    override suspend fun getFortunes(): List<Fortune> {
        val results = fortunesStatement.execute().coAwait()
        return results.map { Fortune(it.getInteger(0), it.getString(1)) }
    }

    override suspend fun getWorld(): World =
        getWorlds(1).first()

    override suspend fun getWorlds(count: Int): List<World> = coroutineScope {
        List(count) {
            async { fetchWorld(nextWorldId()) }
        }.awaitAll()
    }

    private suspend fun fetchWorld(id: Int): World {
        val result = selectWorldStatement.execute(Tuple.of(id)).coAwait()
        val row = result.first()
        return World(row.getInteger(0), row.getInteger(1)!!)
    }

    override suspend fun updateWorlds(worlds: List<World>) {
        if (worlds.isEmpty()) return
        val batch = worlds.sortedBy { it.id }.map { Tuple.of(it.randomNumber, it.id) }
        updateWorldStatement.executeBatch(batch).coAwait()
    }
}

fun String.toBoxedInt(range: IntRange): Int? =
    toIntOrNull()?.coerceIn(range)

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

class FortuneTemplate(
    private val fortunes: List<Fortune>,
    private val main: MainTemplate = MainTemplate()
) : Template<HTML> {
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

fun Application.main() {
    val db = PgclientRepository()

    install(DefaultHeaders)
    routing {
        get("/plaintext") {
            call.respond(TextContent(HELLO_WORLD, ContentType.Text.Plain))
        }

        get("/json") {
            call.respondJson(Message(HELLO_WORLD))
        }

        get("/db") {
            call.respondJson(db.getWorld())
        }

        get("/query") {
            val queries = call.parameters["queries"]?.toBoxedInt(1..500) ?: 1
            val worlds = db.getWorlds(queries)
            call.respondJson(worlds)
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
            val worlds = db.getWorlds(queries).map { world ->
                world.randomNumber = nextWorldId()
                world
            }
            db.updateWorlds(worlds)
            call.respondJson(worlds)
        }
    }
}
