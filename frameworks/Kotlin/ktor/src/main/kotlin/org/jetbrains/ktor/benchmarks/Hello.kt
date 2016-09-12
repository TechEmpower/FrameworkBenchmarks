package org.jetbrains.ktor.benchmarks

import org.jetbrains.ktor.application.*
import org.jetbrains.ktor.jetty.*
import org.jetbrains.ktor.routing.*

fun main(args: Array<String>) {
    embeddedJettyServer(9090) {
        get("/plaintext") { call ->
            call.respond("Hello, World!")
        }
    }
}

