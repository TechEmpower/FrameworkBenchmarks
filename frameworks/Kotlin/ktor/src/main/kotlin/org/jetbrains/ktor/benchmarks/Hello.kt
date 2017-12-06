package org.jetbrains.ktor.benchmarks

import com.google.gson.*
import com.mysql.jdbc.*
import com.zaxxer.hikari.*
import io.ktor.application.*
import io.ktor.content.*
import io.ktor.features.*
import io.ktor.http.*
import io.ktor.response.respond
import io.ktor.response.respondText
import io.ktor.routing.*
import java.util.concurrent.*
import java.util.concurrent.atomic.*
import javax.sql.*
import kotlinx.coroutines.experimental.*

data class Message(val message: String = "Hello, World!")
data class World(val id: Int, var randomNumber: Int)

fun Application.main() {
    val gson = GsonBuilder().create()
    val DbRows = 10000
    Driver::class.java.newInstance()
    val pool by lazy {
        hikari()
    }

    val counter = AtomicInteger()
    val databaseExecutor = Executors.newFixedThreadPool(256) { r ->
        Thread(r, "db-${counter.incrementAndGet()}-thread")
    }
    val databaseDispatcher = databaseExecutor.asCoroutineDispatcher()

    install(DefaultHeaders)

    routing {

        get("/plaintext") {
            call.respondText("Hello, World!", ContentType.Text.Plain)
        }

        get("/json") {
            call.respondText(gson.toJson(Message()), ContentType.Application.Json)
        }

        get("/db") {
            val response = run(databaseDispatcher) {
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

        get("/updates") {
            val t = run(databaseDispatcher) {
                pool.connection.use { connection ->
                    val queries = call.queries()
                    val random = ThreadLocalRandom.current()
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
    config.jdbcUrl = "jdbc:mysql://TFB-database:3306/hello_world?useSSL=false"
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
