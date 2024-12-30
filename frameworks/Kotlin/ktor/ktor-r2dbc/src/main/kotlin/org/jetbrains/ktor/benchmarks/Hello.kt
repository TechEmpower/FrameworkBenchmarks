package org.jetbrains.ktor.benchmarks

import io.ktor.http.*
import io.ktor.http.content.*
import io.ktor.server.application.*
import io.ktor.server.config.*
import io.ktor.server.html.*
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.r2dbc.spi.ConnectionFactories
import io.r2dbc.spi.ConnectionFactory
import io.r2dbc.spi.ConnectionFactoryOptions
import kotlinx.coroutines.*
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

fun Application.main() {
    val config = ApplicationConfig("application.conf")
    val dbConnFactory = configurePostgresR2DBC(config)

    install(DefaultHeaders)

    val helloWorldContent = TextContent("Hello, World!", ContentType.Text.Plain)

    routing {
        get("/plaintext") {
            call.respond(helloWorldContent)
        }

        get("/json") {
            call.respondText(Json.encodeToString(Message("Hello, world!")), ContentType.Application.Json)
        }

        get("/db") {
            val random = Random.Default
            val request = getWorld(dbConnFactory, random)
            val result = request.awaitFirstOrNull()

            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
        }

        suspend fun selectWorlds(queries: Int, random: Random): List<World> = coroutineScope {
            val result = ArrayList<Deferred<World?>>(queries)

            repeat(queries) {
                val deferred = async {
                    getWorld(dbConnFactory, random).awaitFirstOrNull()
                }
                result.add(deferred)
            }

            result.awaitAll().filterNotNull()
        }

        get("/queries") {
            val queries = call.queries()
            val random = Random.Default

            val result = selectWorlds(queries, random)

            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
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

            val result = coroutineScope {
                val worlds = selectWorlds(queries, random)

                worlds.forEach { it.randomNumber = random.nextInt(DB_ROWS) + 1 }

                val updateRequests = worlds.map { world ->
                    Mono.usingWhen(dbConnFactory.create(), { connection ->
                        Mono.from(
                            connection.createStatement(UPDATE_QUERY).bind(0, world.randomNumber).bind(1, world.id)
                                .execute()
                        ).flatMap { Mono.from(it.rowsUpdated) }
                    }, { connection -> connection.close() })
                }

                Flux.merge(updateRequests).collectList().awaitFirstOrNull()
                worlds
            }

            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
        }
    }
}

private fun getWorld(
    dbConnFactory: ConnectionFactory, random: Random
): Mono<World> = Mono.usingWhen(dbConnFactory.create(), { connection ->
    Mono.from(connection.createStatement(WORLD_QUERY).bind(0, random.nextInt(DB_ROWS) + 1).execute()).flatMap { r ->
        Mono.from(r.map { row, _ ->
            World(
                row.get(0, Int::class.java)!!, row.get(1, Int::class.java)!!
            )
        })
    }
}, { connection -> connection.close() })

private fun configurePostgresR2DBC(config: ApplicationConfig): ConnectionFactory {
    val options = ConnectionFactoryOptions.builder().option(ConnectionFactoryOptions.DRIVER, "database.driver")
        .option(ConnectionFactoryOptions.DATABASE, config.property("database.url").getString())
        .option(ConnectionFactoryOptions.USER, config.property("database.user").getString())
        .option(ConnectionFactoryOptions.PASSWORD, config.property("database.password").getString()).build()

    return ConnectionFactories.get(options)
}

private fun ApplicationCall.queries() = request.queryParameters["queries"]?.toIntOrNull()?.coerceIn(1, 500) ?: 1


object Constants {
    const val WORLD_QUERY = "SELECT id, randomNumber FROM World WHERE id = ?"
    const val FORTUNES_QUERY = "SELECT id, message FROM fortune"
    const val UPDATE_QUERY = "UPDATE World SET randomNumber = ? WHERE id = ?"
    const val DB_ROWS = 10000
}
