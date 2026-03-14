package database

import Fortune
import World
import io.vertx.sqlclient.Row

fun Row.toWorld() =
    World(getInteger(0), getInteger(1))

fun Row.toFortune() =
    Fortune(getInteger(0), getString(1))
