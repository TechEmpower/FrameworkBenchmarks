package org.jetbrains.ktor.benchmarks

import com.google.gson.*
import com.mysql.jdbc.*
import com.zaxxer.hikari.*
import kotlinx.coroutines.experimental.*
import org.jetbrains.ktor.application.*
import org.jetbrains.ktor.content.*
import org.jetbrains.ktor.features.*
import org.jetbrains.ktor.http.*
import org.jetbrains.ktor.routing.*
import org.jetbrains.ktor.util.*
import java.sql.ResultSet.*
import java.util.concurrent.*
import java.util.concurrent.atomic.*
import javax.sql.*


data class Message(val message: String = "Hello, World!")
data class World(val id: Int, var randomNumber: Int)

fun Application.main() {
    val (a, b) = hex("62656e63686d61726b6462757365723a62656e63686d61726b646270617373").toString(Charsets.ISO_8859_1).split(":")
    val gson = GsonBuilder().create()
    val DbRows = 10000
    Driver::class.java.newInstance()
    val pool by lazy {
        val dbHost = System.getenv("DBHOST") ?: error("DBHOST environment variable is not set")
        hikari(dbHost, a, b)
    }

    val counter = AtomicInteger()
    val databaseExecutor = Executors.newFixedThreadPool(256) { r ->
        Thread(r, "db-${counter.incrementAndGet()}-thread")
    }
    val databaseDispatcher = databaseExecutor.asCoroutineDispatcher()

    install(SamplingCallLogging) {
        samplingFactor = 50000L
    }

    install(DefaultHeaders)
    routing {
        get("/plaintext") { call ->
            call.respond(TextContent("Hello, World!", ContentType.Text.Plain, HttpStatusCode.OK))
        }
        get("/json") { call ->
            call.respond(TextContent(gson.toJson(Message()), ContentType.Application.Json, HttpStatusCode.OK))
        }
        get("/db") { call ->
            val response = run(databaseDispatcher) {
                pool.connection.use { connection ->
                    val random = ThreadLocalRandom.current()
                    val queries = call.queries()
                    val result = mutableListOf<World>()

                    connection.prepareStatement("SELECT * FROM World WHERE id = ?", TYPE_FORWARD_ONLY, CONCUR_READ_ONLY).use { statement ->
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

                    connection.prepareStatement("SELECT * FROM World WHERE id = ?", TYPE_FORWARD_ONLY, CONCUR_READ_ONLY).use { statement ->
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

private fun hikari(dbHost: String, a: String, b: String): DataSource {
    val config = HikariConfig()
    config.jdbcUrl = "jdbc:mysql://$dbHost:3306/hello_world?useSSL=false"
    config.username = a
    config.password = b
    config.addDataSourceProperty("cachePrepStmts", "true")
    config.addDataSourceProperty("prepStmtCacheSize", "250")
    config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
    config.driverClassName = Driver::class.java.name
    config.connectionTimeout = 10000
    config.maximumPoolSize = 100

    return HikariDataSource(config)
}
