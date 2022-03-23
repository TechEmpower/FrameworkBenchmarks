package io.vertx.benchmark.model

import io.vertx.core.json.JsonObject

/**
 * The model for the "fortune" database table.
 */
class Fortune(val id: Int, val message: String) : Comparable<Fortune> {
    constructor(doc: JsonObject) : this(doc.getInteger("id"), doc.getString("message"))

    override fun compareTo(other: Fortune): Int =
        message compareTo other.message
}