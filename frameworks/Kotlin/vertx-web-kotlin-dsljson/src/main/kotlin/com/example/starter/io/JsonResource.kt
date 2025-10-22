package com.example.starter.io

import io.vertx.core.json.JsonObject

object JsonResource {
    fun of(resource: String): JsonObject {
        val classLoader = ClassLoader.getSystemClassLoader()
        classLoader.getResourceAsStream(resource)?.use { input ->
            val output = BufferOutputStream()
            output.write(input.readAllBytes())
            return output.toJsonObject()
        }
        throw IllegalStateException("$resource not found")
    }
}
