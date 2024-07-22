package com.example.starter.models

import com.dslplatform.json.CompiledJson

@CompiledJson
class Fortune(val id: Int, val message: String) : Comparable<Fortune> {
    override fun compareTo(other: Fortune): Int {
        return message.compareTo(other.message)
    }
}
