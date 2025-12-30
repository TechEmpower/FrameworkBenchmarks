package com.example.starter.models

import com.dslplatform.json.CompiledJson
import com.dslplatform.json.JsonAttribute

@CompiledJson
class World(
    @field:JsonAttribute(nullable = false) val id: Int,
    @field:JsonAttribute(nullable = false) var randomNumber: Int,
) : Comparable<World> {
    override fun compareTo(other: World): Int = id - other.id
}
