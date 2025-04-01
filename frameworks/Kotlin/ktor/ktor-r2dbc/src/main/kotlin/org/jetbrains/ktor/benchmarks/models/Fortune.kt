package org.jetbrains.ktor.benchmarks.models

import kotlinx.serialization.Serializable

@Serializable
data class Fortune(val id: Int, var message: String)
