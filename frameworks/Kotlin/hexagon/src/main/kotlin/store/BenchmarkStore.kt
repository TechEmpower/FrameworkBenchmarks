package com.hexagonkt.store

import com.hexagonkt.Fortune
import com.hexagonkt.World
import java.util.concurrent.ThreadLocalRandom

internal interface BenchmarkStore {
    fun findAllFortunes(): List<Fortune>
    fun findWorlds(count: Int): List<World>
    fun replaceWorlds(count: Int): List<World>
    fun close()

    fun randomWorld(rows: Int): Int = ThreadLocalRandom.current().nextInt(rows) + 1
}