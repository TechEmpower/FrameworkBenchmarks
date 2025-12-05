package org.jetbrains.ktor.benchmarks

import io.ktor.http.*
import io.ktor.http.content.*
import io.ktor.server.application.*
import io.ktor.server.config.*
import io.ktor.server.html.respondHtml
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.r2dbc.pool.ConnectionPool
import io.r2dbc.pool.ConnectionPoolConfiguration
import io.r2dbc.postgresql.PostgresqlConnectionConfiguration
import io.r2dbc.postgresql.PostgresqlConnectionFactory
import io.r2dbc.postgresql.client.SSLMode
import io.r2dbc.spi.Connection
import io.r2dbc.spi.ConnectionFactory
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.reactive.awaitFirst
import kotlinx.coroutines.reactive.awaitFirstOrNull
import kotlinx.coroutines.reactor.awaitSingle
import kotlinx.html.*
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono
import java.time.Duration
import java.util.concurrent.ThreadLocalRandom
import kotlin.random.Random

const val HELLO_WORLD = "Hello, World!"
const val WORLD_QUERY = "SELECT id, randomnumber FROM world WHERE id = $1"
const val FORTUNES_QUERY = "SELECT id, message FROM fortune"
const val UPDATE_QUERY = "UPDATE world SET randomnumber = $1 WHERE id = $2"
const val DB_ROWS = 10000

fun Application.main() {
    val config = ApplicationConfig("application.conf")
    val dbConnFactory = configurePostgresR2DBC(config)

    val helloWorldContent = TextContent("Hello, World!", ContentType.Text.Plain)

    install(DefaultHeaders)

    routing {
        get("/plaintext") {
            call.respond(helloWorldContent)
        }

        get("/json") {
            call.respondJson(Message(HELLO_WORLD))
        }

        get("/db") {
            val request = getWorld(dbConnFactory)
            val result = request.awaitFirstOrNull()

            call.respondJson(result)
        }

        get("/queries") {
            val queries = call.queries()

            val result = fetchWorldsConcurrently(dbConnFactory, queries)

            call.respondJson(result)
        }

        get("/fortunes") {
            val result = mutableListOf<Fortune>()

            val request = Flux.usingWhen(dbConnFactory.create(), { connection ->
                Flux.from(connection.createStatement(FORTUNES_QUERY).execute()).flatMap { r ->
                    Flux.from(r.map { row, _ ->
                        Fortune(
                            row.get(0, Int::class.java)!!, row.get(1, String::class.java)!!
                        )
                    })
                }
            }, { connection -> connection.close() })

            request.collectList().awaitFirstOrNull()?.let { result.addAll(it) }

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

            val worlds = fetchWorldsConcurrently(dbConnFactory, queries)
            val updatedWorlds = worlds.map {
                it.copy(randomNumber = ThreadLocalRandom.current().nextInt(1, DB_ROWS + 1))
            }.sortedBy { it.id }

            Mono.usingWhen(dbConnFactory.create(), { connection ->
                Mono.from(connection.beginTransaction())
                    .thenMany(
                        Flux.fromIterable(updatedWorlds)
                            .concatMap { world ->
                                Mono.from(
                                    connection.createStatement(UPDATE_QUERY)
                                        .bind("$1", world.randomNumber)
                                        .bind("$2", world.id)
                                        .execute()
                                ).flatMap { Mono.from(it.rowsUpdated) }
                            }
                    )
                    .then(Mono.from(connection.commitTransaction()))
            },
                Connection::close,
                { connection, _ -> connection.rollbackTransaction() },
                { connection -> connection.rollbackTransaction() }
            ).awaitFirstOrNull()

            call.respondJson(updatedWorlds)
        }
    }
}

private fun getWorld(
    dbConnFactory: ConnectionFactory, random: ThreadLocalRandom = ThreadLocalRandom.current()
): Mono<World> = Mono.usingWhen(dbConnFactory.create(), { connection ->
    Mono.from(connection.createStatement(WORLD_QUERY)
        .bind("$1", random.nextInt(DB_ROWS) + 1)
        .execute())
        .flatMap { result ->
            Mono.from(result.map { row, _ ->
                World(
                    row.get(0, Int::class.java)
                        ?: error("id is null"),
                    row.get(1, Int::class.java)
                        ?: error("randomNumber is null")
                )
            })
        }
}, Connection::close)

suspend fun fetchWorldsConcurrently(factory: ConnectionFactory, count: Int): List<World> =
    coroutineScope {
        (0 until count).map {
            async { getWorld(factory).awaitSingle() }
        }.awaitAll()
    }

private fun configurePostgresR2DBC(config: ApplicationConfig): ConnectionFactory {
    val cfo = PostgresqlConnectionConfiguration.builder()
        .host(config.property("db.host").getString())
        .port(config.property("db.port").getString().toInt())
        .database(config.property("db.database").getString())
        .username(config.property("db.username").getString())
        .password(config.property("db.password").getString())
        .loopResources { NioClientEventLoopResources(Runtime.getRuntime().availableProcessors()).cacheLoops() }
        .sslMode(SSLMode.DISABLE)
        .tcpKeepAlive(true)
        .tcpNoDelay(true)
        .build()

    val cf = PostgresqlConnectionFactory(cfo)

    val cp = ConnectionPoolConfiguration.builder(cf)
        .initialSize(config.property("db.initPoolSize").getString().toInt())
        .maxSize(config.property("db.maxPoolSize").getString().toInt())
        .maxIdleTime(Duration.ofSeconds(30))
        .maxAcquireTime(Duration.ofSeconds(5))
        .validationQuery("SELECT 1")
        .build()

    return ConnectionPool(cp)
}

private fun ApplicationCall.queries() = request.queryParameters["queries"]?.toIntOrNull()?.coerceIn(1, 500) ?: 1