package com.hexagonkt.store

import com.hexagonkt.CachedWorld
import com.hexagonkt.Fortune
import com.hexagonkt.Settings
import com.hexagonkt.World
import org.cache2k.Cache
import org.cache2k.Cache2kBuilder

internal abstract class BenchmarkStore(settings: Settings) {

    abstract fun findAllFortunes(): List<Fortune>
    abstract fun findWorlds(ids: List<Int>): List<World>
    abstract fun replaceWorlds(worlds: List<World>)
    abstract fun initWorldsCache(cache: Cache<Int, CachedWorld>)
    abstract fun loadCachedWorld(id: Int): CachedWorld
    abstract fun close()

    private val worldsCache: Cache<Int, CachedWorld> by lazy {
        object : Cache2kBuilder<Int, CachedWorld>() {}
            .eternal(true)
            .disableMonitoring(true)
            .disableStatistics(true)
            .entryCapacity(settings.worldRows.toLong())
            .loader { id -> loadCachedWorld(id) }
            .build()
            .apply { initWorldsCache(this) }
    }

    fun findCachedWorlds(ids: List<Int>): List<CachedWorld> {
        return ids.mapNotNull { worldsCache.get(it) }
    }
}