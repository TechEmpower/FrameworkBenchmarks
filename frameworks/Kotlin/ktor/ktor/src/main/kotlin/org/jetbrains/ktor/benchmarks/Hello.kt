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
import kotlinx.coroutines.*
import kotlinx.html.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import org.jetbrains.ktor.benchmarks.Constants.FORTUNES_QUERY
import org.jetbrains.ktor.benchmarks.Constants.UPDATE_QUERY
import org.jetbrains.ktor.benchmarks.Constants.WORLD_QUERY
import java.sql.Connection
import java.util.concurrent.ThreadLocalRandom
import kotlin.random.Random
import kotlinx.serialization.Contextual

@Serializable
data class Message(val message: String)

@Serializable
data class World(val id: Int, var randomNumber: Int)

@Serializable
data class Fortune(val id: Int, var message: String)

// Optimized JSON instance with better performance settings
private val json = Json {
    prettyPrint = false
    isLenient = true
    ignoreUnknownKeys = true
    coerceInputValues = true
}

fun Application.main() {
    val dbRows = 10000
    val poolSize = Runtime.getRuntime().availableProcessors() * 2
    val pool = HikariDataSource(HikariConfig().apply { configurePostgres(poolSize) })
    
    // Create a dedicated dispatcher for database operations
    val databaseDispatcher = Dispatchers.IO.limitedParallelism(poolSize)

    install(DefaultHeaders)

    val helloWorldContent = TextContent("Hello, World!", ContentType.Text.Plain)
    val jsonResponse = json.encodeToString(Message("Hello, world!"))

    routing {
        get("/plaintext") {
            call.respond(helloWorldContent)
        }

        get("/json") {
            call.respondText(jsonResponse, ContentType.Application.Json)
        }

        get("/db") {
            val random = Random.Default

            val world = withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    connection.prepareStatement(WORLD_QUERY).use { statement ->
                        statement.setInt(1, random.nextInt(dbRows) + 1)
                        statement.executeQuery().use { rs ->
                            rs.next()
                            World(rs.getInt(1), rs.getInt(2))
                        }
                    }
                }
            }

            call.respondText(json.encodeToString(world), ContentType.Application.Json)
        }

        fun Connection.selectWorlds(queries: Int, random: Random): List<World> {
            val result = ArrayList<World>(queries)
            prepareStatement(WORLD_QUERY).use { statement ->
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
            val random = Random.Default

            val result = withContext(databaseDispatcher) {
                pool.connection.use { it.selectWorlds(queries, random) }
            }

            call.respondText(json.encodeToString(result), ContentType.Application.Json)
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
            val random = Random.Default
            val result: List<World>

            withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    result = connection.selectWorlds(queries, random)
                    result.forEach { it.randomNumber = random.nextInt(dbRows) + 1 }

                    connection.prepareStatement(UPDATE_QUERY).use { updateStatement ->
                        for ((id, randomNumber) in result) {
                            updateStatement.setInt(1, randomNumber)
                            updateStatement.setInt(2, id)
                            updateStatement.addBatch()
                        }
                        updateStatement.executeBatch()
                    }
                }
            }

            call.respondText(json.encodeToString(result), ContentType.Application.Json)
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
    connectionTimeout = 5000 
    maximumPoolSize = poolSize
    minimumIdle = poolSize
    idleTimeout = 300000 // 5 minutes
    maxLifetime = 600000 // 10 minutes
    validationTimeout = 5000
    leakDetectionThreshold = 60000
}

fun HikariConfig.configureMySql(poolSize: Int) {
    jdbcUrl = "jdbc:mysql://tfb-database:3306/hello_world?useSSL=false"
    driverClassName = com.mysql.jdbc.Driver::class.java.name
    configureCommon(poolSize)
}

fun ApplicationCall.queries() =
    request.queryParameters["queries"]?.toIntOrNull()?.coerceIn(1, 500) ?: 1

object Constants {
    const val WORLD_QUERY = "SELECT id, randomNumber FROM World WHERE id = ?"
    const val FORTUNES_QUERY = "SELECT id, message FROM fortune"
    const val UPDATE_QUERY = "UPDATE World SET randomNumber = ? WHERE id = ?"
}