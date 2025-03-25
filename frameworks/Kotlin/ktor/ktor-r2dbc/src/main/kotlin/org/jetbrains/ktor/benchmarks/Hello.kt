package org.jetbrains.ktor.benchmarks

import io.ktor.http.*
import io.ktor.http.content.*
import io.ktor.server.application.*
import io.ktor.server.config.*
import io.ktor.server.html.*
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
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.reactive.awaitFirst
import kotlinx.coroutines.reactive.awaitFirstOrNull
import kotlinx.html.*
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import org.jetbrains.ktor.benchmarks.Constants.DB_ROWS
import org.jetbrains.ktor.benchmarks.Constants.FORTUNES_QUERY
import org.jetbrains.ktor.benchmarks.Constants.UPDATE_QUERY
import org.jetbrains.ktor.benchmarks.Constants.WORLD_QUERY
import org.jetbrains.ktor.benchmarks.models.Fortune
import org.jetbrains.ktor.benchmarks.models.Message
import org.jetbrains.ktor.benchmarks.models.World
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono
import kotlin.random.Random
import java.time.Duration

private val json = Json {
    prettyPrint = false
    isLenient = true
    ignoreUnknownKeys = true
    coerceInputValues = true
}

fun Application.main() {
    val config = ApplicationConfig("application.conf")
    val dbConnFactory = configurePostgresR2DBC(config)

    install(DefaultHeaders)

    val helloWorldContent = TextContent("Hello, World!", ContentType.Text.Plain)
    val helloWorldMsg = Message("Hello, world!")

    routing {
        get("/plaintext") {
            call.respond(helloWorldContent)
        }

        get("/json") {
            call.respondText(json.encodeToString(helloWorldMsg), ContentType.Application.Json)
        }

        get("/db") {
            val random = Random.Default
            val request = getWorld(dbConnFactory, random)
            val result = request.awaitFirstOrNull()

            call.respondText(json.encodeToString(result), ContentType.Application.Json)
        }

        fun selectWorlds(queries: Int, random: Random): Flow<World> = flow {
            repeat(queries) {
                emit(getWorld(dbConnFactory, random).awaitFirst())
            }
        }

        get("/queries") {
            val queries = call.queries()
            val random = Random.Default

            val result = buildList {
                selectWorlds(queries, random).collect {
                    add(it)
                }
            }

            call.respondText(json.encodeToString(result), ContentType.Application.Json)
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
            val random = Random.Default

            val worlds = selectWorlds(queries, random)

            val worldsUpdated = buildList {
                worlds.collect { world ->
                    world.randomNumber = random.nextInt(DB_ROWS) + 1
                    add(world)

                    Mono.usingWhen(dbConnFactory.create(), { connection ->
                        Mono.from(
                            connection.createStatement(UPDATE_QUERY)
                                .bind(0, world.randomNumber)
                                .bind(1, world.id)
                                .execute()
                        ).flatMap { Mono.from(it.rowsUpdated) }
                    }, Connection::close).awaitFirstOrNull()
                }
            }

            call.respondText(json.encodeToString(worldsUpdated), ContentType.Application.Json)
        }
    }
}

private fun getWorld(
    dbConnFactory: ConnectionFactory, random: Random
): Mono<World> = Mono.usingWhen(dbConnFactory.create(), { connection ->
    Mono.from(connection.createStatement(WORLD_QUERY)
        .bind("$1", random.nextInt(DB_ROWS) + 1)
        .execute())
        .flatMap { r ->
            Mono.from(r.map { row, _ ->
                val id = row.get(0, Int::class.java)
                val randomNumber = row.get(1, Int::class.java)
                if (id != null && randomNumber != null) {
                    World(id, randomNumber)
                } else {
                    throw IllegalStateException("Database returned null values for required fields")
                }
            })
        }
}, Connection::close)

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


object Constants {
    const val WORLD_QUERY = "SELECT id, randomnumber FROM world WHERE id = $1"
    const val FORTUNES_QUERY = "SELECT id, message FROM fortune"
    const val UPDATE_QUERY = "UPDATE world SET randomnumber = $1 WHERE id = $2"
    const val DB_ROWS = 10000
}
