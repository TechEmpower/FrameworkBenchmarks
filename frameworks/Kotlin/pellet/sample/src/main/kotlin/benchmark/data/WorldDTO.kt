package benchmark.data

import kotlinx.serialization.Serializable

@Serializable
data class WorldDTO(val id: Int, val randomNumber: Int)