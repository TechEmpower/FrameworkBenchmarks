package database

import Fortune
import World
import io.r2dbc.spi.Readable

fun Readable.toWorld() =
    World(get(0, Int::class.java)!!, get(1, Int::class.java)!!)

fun Readable.toFortune() =
    Fortune(get(0, Int::class.java)!!, get(1, String::class.java)!!)
