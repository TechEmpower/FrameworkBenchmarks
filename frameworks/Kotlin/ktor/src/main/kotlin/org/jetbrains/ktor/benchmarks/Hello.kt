package org.jetbrains.ktor.benchmarks

import com.google.gson.*
import com.mchange.v2.c3p0.*
import com.mysql.cj.jdbc.*
import org.jetbrains.ktor.application.*
import org.jetbrains.ktor.content.*
import org.jetbrains.ktor.features.*
import org.jetbrains.ktor.host.*
import org.jetbrains.ktor.http.*
import org.jetbrains.ktor.netty.*
import org.jetbrains.ktor.routing.*
import org.jetbrains.ktor.util.*
import java.sql.ResultSet.CONCUR_READ_ONLY
import java.sql.ResultSet.TYPE_FORWARD_ONLY
import java.util.*

data class Message(val message: String = "Hello, World!")
data class World(val id: Int, var randomNumber: Int)

fun main(args: Array<String>) {
    val (a, b) = hex("726f6f743a736563726574").toString(Charsets.ISO_8859_1).split(":")
    val gson = GsonBuilder().create()
    val DbRows = 10000
    val dbHost = System.getenv("DBHOST")
    Driver::class.java.newInstance()
    val pool = ComboPooledDataSource().apply {
        driverClass = Driver::class.java.name
        jdbcUrl = "jdbc:mysql://$dbHost:3306/hello_world"
        user = a
        password = b
    }

    embeddedServer(Netty, 9090) {
        install(DefaultHeaders)
        routing {
            get("/plaintext") { call ->
                call.respond(TextContent("Hello, World!", ContentType.Text.Plain, HttpStatusCode.OK))
            }
            get("/json") { call ->
                call.respond(TextContent(gson.toJson(Message()), ContentType.Application.Json, HttpStatusCode.OK))
            }
            get("/db") { call ->
                pool.connection.use { connection ->
                    val queries = call.queries()
                    val random = Random()
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

                        call.respond(TextContent(gson.toJson(when (queries) {
                            null -> result.single()
                            else -> result
                        }), ContentType.Application.Json, HttpStatusCode.OK))
                    }
                }
            }
            get("/updates") {
                pool.connection.use { connection ->
                    val queries = call.queries()
                    val random = Random()
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

                    call.respond(TextContent(gson.toJson(when (queries) {
                        null -> result.single()
                        else -> result
                    }), ContentType.Application.Json, HttpStatusCode.OK))
                }
            }
        }
    }.start(true)
}

fun ApplicationCall.queries() = try {
    request.queryParameters["queries"]?.toInt()?.coerceIn(1, 500)
} catch (nfe: NumberFormatException) {
    1
}

