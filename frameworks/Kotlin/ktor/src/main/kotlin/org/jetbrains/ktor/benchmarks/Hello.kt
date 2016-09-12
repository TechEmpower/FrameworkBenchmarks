package org.jetbrains.ktor.benchmarks

import com.google.gson.*
import org.jetbrains.ktor.features.*
import org.jetbrains.ktor.features.http.*
import org.jetbrains.ktor.http.*
import org.jetbrains.ktor.netty.*
import org.jetbrains.ktor.routing.*
import org.jetbrains.ktor.transform.*

data class Message(val message: String = "Hello, World!")

fun main(args: Array<String>) {
    val gson = GsonBuilder().create()

    embeddedNettyServer(9090) {
        application.install(DefaultHeaders)

        get("/plaintext") { call ->
            call.respond(TextContentResponse(HttpStatusCode.OK, ContentType.Text.Plain, "Hello, World!"))
        }
        get("/json") { call ->
            call.respond(TextContentResponse(HttpStatusCode.OK, ContentType.Application.Json, gson.toJson(Message())))
        }
    }.start(true)
}

