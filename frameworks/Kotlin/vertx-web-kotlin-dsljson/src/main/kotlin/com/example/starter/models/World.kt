package com.example.starter.models

import com.dslplatform.json.CompiledJson
import com.dslplatform.json.JsonAttribute

@CompiledJson
class World(
    @JsonAttribute(nullable = false) val id: Int,
    @JsonAttribute(nullable = false) var randomNumber: Int
) : Comparable<World> {
    override fun compareTo(other: World): Int {
        return id.compareTo(other.id)
    }
}
