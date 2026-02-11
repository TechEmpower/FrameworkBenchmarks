package org.jetbrains.ktor.benchmarks

import kotlinx.serialization.Serializable

@Serializable
data class Message(val message: String)

@Serializable
data class World(val id: Int, var randomNumber: Int)

@Serializable
data class Fortune(val id: Int, var message: String)