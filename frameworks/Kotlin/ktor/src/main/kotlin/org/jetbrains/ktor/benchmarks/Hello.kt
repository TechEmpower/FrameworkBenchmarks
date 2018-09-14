package org.jetbrains.ktor.benchmarks

import com.zaxxer.hikari.*
import io.ktor.application.*
import io.ktor.content.*
import io.ktor.features.*
import io.ktor.html.*
import io.ktor.http.*
import io.ktor.response.*
import io.ktor.routing.*
import kotlinx.coroutines.experimental.*
import kotlinx.coroutines.experimental.scheduling.*
import kotlinx.html.*
import kotlinx.serialization.*
import kotlinx.serialization.json.*
import java.util.concurrent.*

@Serializable
data class Message(val message: String = "Hello, World!")

@Serializable
data class World(val id: Int, var randomNumber: Int)

@Serializable
data class Fortune(val id: Int, var message: String)

fun Application.main() {
    val worldSerializer = World.serializer()
    val worldListSerializer = World.serializer().list

    val dbRows = 10000
    val poolSize = 48
    val pool by lazy { HikariDataSource(HikariConfig().apply { configurePostgres(poolSize) }) }
    val databaseDispatcher by lazy { ExperimentalCoroutineDispatcher().blocking(poolSize) }

    install(DefaultHeaders)

    val okContent = TextContent("Hello, World!", ContentType.Text.Plain, HttpStatusCode.OK).also { it.contentLength }

    routing {
        get("/plaintext") {
            call.respond(okContent)
        }

        get("/json") {
            call.respondText(JSON.stringify(Message()), ContentType.Application.Json, HttpStatusCode.OK)
        }

        get("/db") {
            val random = ThreadLocalRandom.current()
            val queries = call.queries()
            val result = ArrayList<World>(queries ?: 1)

            withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    connection.prepareStatement("SELECT id, randomNumber FROM World WHERE id = ?").use { statement ->
                        for (i in 1..(queries ?: 1)) {
                            statement.setInt(1, random.nextInt(dbRows) + 1)
                            statement.executeQuery().use { rs ->
                                while (rs.next()) {
                                    result += World(rs.getInt(1), rs.getInt(2))
                                }
                            }
                        }
                    }
                }
            }

            call.respondText(when (queries) {
                null -> JSON.stringify(worldSerializer, result.single())
                else -> JSON.stringify(worldListSerializer, result)
            }, ContentType.Application.Json, HttpStatusCode.OK)
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
            val result = ArrayList<World>(queries ?: 1)

            withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    connection.prepareStatement("SELECT id, randomNumber FROM World WHERE id = ?").use { statement ->
                        for (i in 1..(queries ?: 1)) {
                            statement.setInt(1, random.nextInt(dbRows) + 1)

                            statement.executeQuery().use { rs ->
                                while (rs.next()) {
                                    result += World(rs.getInt(1), rs.getInt(2))
                                }
                            }
                        }

                    }

                    result.forEach { it.randomNumber = random.nextInt(dbRows) + 1 }

                    connection.prepareStatement("UPDATE World SET randomNumber = ? WHERE id = ?").use { updateStatement ->
                        for ((id, randomNumber) in result) {
                            updateStatement.setInt(1, randomNumber)
                            updateStatement.setInt(2, id)

                            updateStatement.executeUpdate()
                        }
                    }

                }
            }

            call.respondText(when (queries) {
                null -> JSON.stringify(worldSerializer, result.single())
                else -> JSON.stringify(worldListSerializer, result)
            }, ContentType.Application.Json, HttpStatusCode.OK)
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

fun ApplicationCall.queries() = try {
    request.queryParameters["queries"]?.toInt()?.coerceIn(1, 500)
} catch (nfe: NumberFormatException) {
    1
}

