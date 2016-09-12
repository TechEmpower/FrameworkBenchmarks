package org.jetbrains.ktor.benchmarks

import org.jetbrains.ktor.features.*
import org.jetbrains.ktor.features.http.*
import org.jetbrains.ktor.http.*
import org.jetbrains.ktor.netty.*
import org.jetbrains.ktor.routing.*
import org.jetbrains.ktor.transform.*

fun main(args: Array<String>) {
    embeddedNettyServer(9090) {
        application.install(DefaultHeaders)

        get("/plaintext") { call ->
            call.respond(TextContentResponse(HttpStatusCode.OK, ContentType.Text.Plain, "Hello, World!"))
        }
    }.start(true)
}

