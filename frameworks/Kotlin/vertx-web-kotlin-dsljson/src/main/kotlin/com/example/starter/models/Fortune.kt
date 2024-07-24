package com.example.starter.models

import com.dslplatform.json.CompiledJson
import com.dslplatform.json.JsonAttribute

@CompiledJson
class Fortune(
    @JsonAttribute(nullable = false) val id: Int,
    @JsonAttribute(nullable = false) val message: String
) : Comparable<Fortune> {
    override fun compareTo(other: Fortune): Int {
        return message.compareTo(other.message)
    }
}
