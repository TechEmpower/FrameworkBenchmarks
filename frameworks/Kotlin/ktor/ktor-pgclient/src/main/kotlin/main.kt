import io.ktor.application.*
import io.ktor.features.*
import io.ktor.html.*
import io.ktor.http.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.vertx.kotlin.coroutines.await
import io.vertx.pgclient.PgConnectOptions
import io.vertx.pgclient.PgPool
import io.vertx.sqlclient.PoolOptions
import io.vertx.sqlclient.Tuple
import kotlinx.html.HTML
import kotlinx.html.HtmlBlockTag
import kotlinx.html.body
import kotlinx.html.head
import kotlinx.html.table
import kotlinx.html.td
import kotlinx.html.th
import kotlinx.html.title
import kotlinx.html.tr
import kotlinx.serialization.Serializable
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json
import java.util.concurrent.ThreadLocalRandom

@Serializable
data class Message(val message: String)

@Serializable
data class World(val id: Int, val randomNumber: Int)

data class Fortune(val id: Int, val message: String)

val rand: ThreadLocalRandom
    get() = ThreadLocalRandom.current()

interface Repository {
    suspend fun getWorld(): World
    suspend fun getFortunes(): List<Fortune>
    suspend fun updateWorlds(worlds: List<World>)
}

class PgclientRepository : Repository {
    private val connectOptions =
        PgConnectOptions()
            .setPort(5432)
            .setHost("tfb-database")
            .setDatabase("hello_world")
            .setUser("benchmarkdbuser")
            .setPassword("benchmarkdbpass")
            .apply {
                cachePreparedStatements = true
            }

    private val poolOptions = PoolOptions()
    private val client = ThreadLocal.withInitial { PgPool.client(connectOptions, poolOptions) }
    private fun client() = client.get()


    override suspend fun getFortunes(): List<Fortune> {
        val results = client().preparedQuery("select id, message from fortune").execute().await()
        return results.map { Fortune(it.getInteger(0), it.getString(1)) }
    }

    override suspend fun getWorld(): World {
        val worldId = rand.nextInt(1, 10001)
        val result =
            client()
                .preparedQuery("select id, randomNumber from world where id = $1")
                .execute(Tuple.of(worldId))
                .await()
        val row = result.first()
        return World(row.getInteger(0), row.getInteger(1)!!)
    }

    override suspend fun updateWorlds(worlds: List<World>) {
        val batch = worlds.map { Tuple.of(it.id, it.randomNumber) }
        client()
            .preparedQuery("update world set randomNumber = $1 where id = $2")
            .executeBatch(batch)
            .await()
    }
}

fun String.toBoxedInt(range: IntRange): Int =
    try {
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

class FortuneTemplate(
    private val fortunes: List<Fortune>,
    private val main: MainTemplate = MainTemplate()
): Template<HTML> {
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

fun main() {
    val db = PgclientRepository()

    val messageSerializer = Message.serializer()
    val worldSerializer = World.serializer()
    val worldListSerializer = ListSerializer(World.serializer())

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
                    Json.encodeToString(messageSerializer, Message("Hello, World!")),
                    ContentType.Application.Json
                )
            }

            get("/db") {
                call.respondText(Json.encodeToString(worldSerializer, db.getWorld()), ContentType.Application.Json)
            }

            get("/query") {
                val queries = call.parameters["queries"]?.toBoxedInt(1..500) ?: 1
                val worlds = (1..queries).map { db.getWorld() }
                call.respondText(Json.encodeToString(worldListSerializer, worlds), ContentType.Application.Json)
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
                val newWorlds = worlds.map { it.copy(randomNumber = rand.nextInt(1, 10001)) }

                db.updateWorlds(newWorlds)

                call.respondText(Json.encodeToString(worldListSerializer, newWorlds), ContentType.Application.Json)
            }
        }
    }

    server.start(wait = true)
}
