package io.vertx.benchmark.model

import io.vertx.core.json.JsonObject

/**
 * The model for the "world" database table.
 */
class World(val id: Int, val randomNumber: Int) : Comparable<World> {
    constructor(doc: JsonObject) : this(doc.getInteger("id"), doc.getInteger("randomNumber"))

    override fun compareTo(other: World): Int =
        id compareTo other.id
}