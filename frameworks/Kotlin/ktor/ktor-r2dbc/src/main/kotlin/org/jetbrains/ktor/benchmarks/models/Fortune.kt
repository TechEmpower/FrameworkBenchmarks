package org.jetbrains.ktor.benchmarks.models

import kotlinx.serialization.Serializable

@Serializable
class Fortune(val id: Int, var message: String)
