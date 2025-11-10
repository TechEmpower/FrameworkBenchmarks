package org.jetbrains.ktor.benchmarks

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import io.ktor.http.*
import io.ktor.http.content.*
import io.ktor.server.application.*
import io.ktor.server.html.respondHtml
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.withContext
import kotlinx.html.*
import java.sql.Connection
import java.util.StringJoiner
import kotlin.random.Random

const val HELLO_WORLD = "Hello, World!"
const val WORLD_QUERY = "SELECT id, randomNumber FROM World WHERE id = ?"
const val FORTUNES_QUERY = "SELECT id, message FROM fortune"
const val DB_ROWS = 10_000

@OptIn(ExperimentalCoroutinesApi::class)
fun Application.main() {
    val poolSize = Runtime.getRuntime().availableProcessors() * 2
    val pool = HikariDataSource(HikariConfig().apply { configurePostgres(poolSize) })

    // Create a dedicated dispatcher for database operations
    val databaseDispatcher = Dispatchers.IO.limitedParallelism(poolSize)
    val helloWorldContent = TextContent(HELLO_WORLD, ContentType.Text.Plain)
    val random = Random.Default

    install(DefaultHeaders)

    routing {
        get("/plaintext") {
            call.respond(helloWorldContent)
        }

        get("/json") {
            call.respondJson(Message(HELLO_WORLD))
        }

        get("/db") {
            val world = withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    connection.prepareStatement(WORLD_QUERY).use { statement ->
                        statement.setInt(1, random.nextInt(DB_ROWS) + 1)
                        statement.executeQuery().use { rs ->
                            rs.next()
                            World(rs.getInt(1), rs.getInt(2))
                        }
                    }
                }
            }
            call.respondJson(world)
        }

        fun Connection.selectWorlds(queries: Int): Array<World> =
            prepareStatement(WORLD_QUERY).use { statement ->
                Array<World>(queries) { i ->
                    statement.setInt(1, random.nextInt(DB_ROWS) + 1)
                    statement.executeQuery().use { rs ->
                        rs.next()
                        World(rs.getInt(1), rs.getInt(2))
                    }
                }
            }

        get("/queries") {
            val queries = call.queries()
            val result = withContext(databaseDispatcher) {
                pool.connection.use { it.selectWorlds(queries) }
            }
            call.respondJson(result)
        }

        get("/fortunes") {
            val result = mutableListOf<Fortune>()
            withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    connection.prepareStatement(FORTUNES_QUERY).use { statement ->
                        statement.executeQuery().use { rs ->
                            while (rs.next()) {
                                result += Fortune(rs.getInt(1), rs.getString(2))
                            }
                        }
                    }
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
            val result: Array<World>

            withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    result = connection.selectWorlds(queries)

                    val updateSql = StringJoiner(
                        ", ",
                        "UPDATE World SET randomNumber = temp.randomNumber FROM (VALUES ",
                        " ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = World.id"
                    )

                    for (i in result.indices) {
                        result[i].randomNumber = random.nextInt(DB_ROWS) + 1
                        updateSql.add("(?, ?)")
                    }

                    connection.prepareStatement(updateSql.toString()).use { statement ->
                        var paramIndex = 0
                        for (world in result) {
                            statement.setInt(++paramIndex, world.id)
                            statement.setInt(++paramIndex, world.randomNumber)
                        }
                        statement.executeUpdate()
                    }
                }
            }

            call.respondJson(result)
        }
    }
}


fun ApplicationCall.queries() =
    request.queryParameters["queries"]?.toIntOrNull()?.coerceIn(1, 500) ?: 1
