package org.jetbrains.ktor.benchmarks.models

import kotlinx.serialization.Serializable

@Serializable
data class Message(val message: String)

// Cache common messages to reduce allocations
object MessageCache {
    val helloWorld = Message("Hello, world!")
}
