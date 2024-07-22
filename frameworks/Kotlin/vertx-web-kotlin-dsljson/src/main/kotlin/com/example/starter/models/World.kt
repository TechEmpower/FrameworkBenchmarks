package com.example.starter.models

import com.dslplatform.json.CompiledJson

@CompiledJson
class World(val id: Int, var randomNumber: Int) : Comparable<World> {
    override fun compareTo(other: World): Int {
        return id.compareTo(other.id)
    }
}
