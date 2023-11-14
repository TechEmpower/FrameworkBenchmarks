package org.jetbrains.ktor.benchmarks

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import io.ktor.http.*
import io.ktor.http.content.*
import io.ktor.server.application.*
import io.ktor.server.html.*
import io.ktor.server.plugins.defaultheaders.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.html.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import java.sql.Connection
import java.util.concurrent.ThreadLocalRandom

@Serializable
data class Message(val message: String)

@Serializable
data class World(val id: Int, var randomNumber: Int)

@Serializable
data class Fortune(val id: Int, var message: String)

fun Application.main() {
    val dbRows = 10000
    val poolSize = 48
    val pool by lazy { HikariDataSource(HikariConfig().apply { configurePostgres(poolSize) }) }
    val databaseDispatcher = Dispatchers.IO

    install(DefaultHeaders)

    val helloWorldContent = TextContent("Hello, World!", ContentType.Text.Plain).also { it.contentLength }

    routing {
        get("/plaintext") {
            call.respond(helloWorldContent)
        }

        get("/json") {
            call.respondText(Json.encodeToString(Message("Hello, world!")), ContentType.Application.Json)
        }

        get("/db") {
            val random = ThreadLocalRandom.current()

            val world = withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    connection.prepareStatement("SELECT id, randomNumber FROM World WHERE id = ?").use { statement ->
                        statement.setInt(1, random.nextInt(dbRows) + 1)

                        statement.executeQuery().use { rs ->
                            rs.next()
                            World(rs.getInt(1), rs.getInt(2))
                        }
                    }
                }
            }

            call.respondText(Json.encodeToString(world), ContentType.Application.Json)
        }

        fun Connection.selectWorlds(queries: Int, random: ThreadLocalRandom): List<World> {
            val result = ArrayList<World>(queries)
            prepareStatement("SELECT id, randomNumber FROM World WHERE id = ?").use { statement ->
                repeat(queries) {
                    statement.setInt(1, random.nextInt(dbRows) + 1)

                    statement.executeQuery().use { rs ->
                        rs.next()
                        result += World(rs.getInt(1), rs.getInt(2))
                    }
                }
            }

            return result
        }

        get("/queries") {
            val queries = call.queries()
            val random = ThreadLocalRandom.current()

            val result = withContext(databaseDispatcher) {
                pool.connection.use { it.selectWorlds(queries, random) }
            }

            call.respondText(Json.encodeToString(result), ContentType.Application.Json)
        }

        get("/fortunes") {
            val result = mutableListOf<Fortune>()
            withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    connection.prepareStatement("SELECT id, message FROM fortune").use { statement ->
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
            val random = ThreadLocalRandom.current()
            val result: List<World>

            withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    result = connection.selectWorlds(queries, random)

                    result.forEach { it.randomNumber = random.nextInt(dbRows) + 1 }

                    connection.prepareStatement("UPDATE World SET randomNumber = ? WHERE id = ?")
                        .use { updateStatement ->
                            for ((id, randomNumber) in result) {
                                updateStatement.setInt(1, randomNumber)
                                updateStatement.setInt(2, id)

                                updateStatement.executeUpdate()
                            }
                        }
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

fun HikariConfig.configureMySql(poolSize: Int) {
    jdbcUrl = "jdbc:mysql://tfb-database:3306/hello_world?useSSL=false"
    driverClassName = com.mysql.jdbc.Driver::class.java.name
    configureCommon(poolSize)
}

fun ApplicationCall.queries() =
    request.queryParameters["queries"]?.toIntOrNull()?.coerceIn(1, 500) ?: 1
