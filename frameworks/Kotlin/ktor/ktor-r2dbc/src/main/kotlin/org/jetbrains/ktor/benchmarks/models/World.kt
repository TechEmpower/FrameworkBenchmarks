package org.jetbrains.ktor.benchmarks.models

import kotlinx.serialization.Serializable

@Serializable
data class World(val id: Int, var randomNumber: Int)
