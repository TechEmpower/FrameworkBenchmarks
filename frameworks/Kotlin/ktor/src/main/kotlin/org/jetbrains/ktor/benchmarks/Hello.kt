package org.jetbrains.ktor.benchmarks

import com.google.gson.*
import com.mysql.jdbc.*
import com.zaxxer.hikari.*
import io.ktor.application.*
import io.ktor.content.*
import io.ktor.features.*
import io.ktor.html.*
import io.ktor.http.*
import io.ktor.response.*
import io.ktor.routing.*
import kotlinx.coroutines.experimental.*
import kotlinx.html.*
import java.util.concurrent.*
import java.util.concurrent.atomic.*
import javax.sql.*

data class Message(val message: String = "Hello, World!")
data class World(val id: Int, var randomNumber: Int)
data class Fortune(val id: Int, var message: String)

fun Application.main() {
    val gson = GsonBuilder().create()
    val DbRows = 10000
    val pool by lazy {
        hikari()
    }

    val databaseDispatcher by lazy {
        val counter = AtomicInteger()
        Executors.newFixedThreadPool(100) { r -> Thread(r, "db-${counter.incrementAndGet()}-thread") }.asCoroutineDispatcher()
    }

    install(DefaultHeaders)

    val okContent = TextContent("Hello, World!", ContentType.Text.Plain, HttpStatusCode.OK).also { it.contentLength }

    routing {
        get("/plaintext") {
            call.respond(okContent)
        }

        get("/json") {
            val content = TextContent(gson.toJson(Message()), ContentType.Application.Json, HttpStatusCode.OK)
            call.respond(content)
        }

        get("/db") {
            val response = withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    val random = ThreadLocalRandom.current()
                    val queries = call.queries()
                    val result = mutableListOf<World>()

                    connection.prepareStatement("SELECT * FROM World WHERE id = ?").use { statement ->
                        for (i in 1..(queries ?: 1)) {
                            statement.setInt(1, random.nextInt(DbRows) + 1)

                            statement.executeQuery().use { rs ->
                                while (rs.next()) {
                                    result += World(rs.getInt("id"), rs.getInt("randomNumber"))
                                }
                            }
                        }

                        TextContent(gson.toJson(when (queries) {
                            null -> result.single()
                            else -> result
                        }), ContentType.Application.Json, HttpStatusCode.OK)
                    }
                }
            }

            call.respond(response)
        }

        get("/fortunes") {
            val result = mutableListOf<Fortune>()
            withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    connection.prepareStatement("select id, message from fortune").use { statement ->
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
            val t = withContext(databaseDispatcher) {
                pool.connection.use { connection ->
                    val queries = call.queries()
                    val random = ThreadLocalRandom.current()
                    val result = mutableListOf<World>()

                    connection.prepareStatement("SELECT id, randomNumber FROM World WHERE id = ?").use { statement ->
                        for (i in 1..(queries ?: 1)) {
                            statement.setInt(1, random.nextInt(DbRows) + 1)

                            statement.executeQuery().use { rs ->
                                while (rs.next()) {
                                    result += World(rs.getInt(1), rs.getInt(2))
                                }
                            }
                        }

                    }

                    result.forEach { it.randomNumber = random.nextInt(DbRows) + 1 }

                    connection.prepareStatement("UPDATE World SET randomNumber = ? WHERE id = ?").use { updateStatement ->
                        for ((id, randomNumber) in result) {
                            updateStatement.setInt(1, randomNumber)
                            updateStatement.setInt(2, id)

                            updateStatement.executeUpdate()
                        }
                    }

                    TextContent(gson.toJson(when (queries) {
                        null -> result.single()
                        else -> result
                    }), ContentType.Application.Json, HttpStatusCode.OK)
                }
            }

            call.respond(t)
        }
    }
}

fun ApplicationCall.queries() = try {
    request.queryParameters["queries"]?.toInt()?.coerceIn(1, 500)
} catch (nfe: NumberFormatException) {
    1
}

private fun hikari(): DataSource {
    val config = HikariConfig()
    config.jdbcUrl = "jdbc:mysql://tfb-database:3306/hello_world?useSSL=false"
    config.username = "benchmarkdbuser"
    config.password = "benchmarkdbpass"
    config.addDataSourceProperty("cachePrepStmts", "true")
    config.addDataSourceProperty("prepStmtCacheSize", "250")
    config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
    config.driverClassName = Driver::class.java.name
    config.connectionTimeout = 10000
    config.maximumPoolSize = 100
    return HikariDataSource(config)
}
