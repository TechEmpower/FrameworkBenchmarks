package com.hexagonkt.async.store

import com.hexagonkt.model.CachedWorld
import com.hexagonkt.model.Fortune
import com.hexagonkt.Settings
import com.hexagonkt.model.World
import org.cache2k.Cache
import org.cache2k.Cache2kBuilder
import java.util.concurrent.CompletableFuture

abstract class BenchmarkStore(settings: Settings) {

    abstract fun findAllFortunes(): CompletableFuture<List<Fortune>>
    abstract fun findWorlds(ids: List<Int>): CompletableFuture<List<World>>
    abstract fun replaceWorlds(worlds: List<World>): CompletableFuture<*>
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
