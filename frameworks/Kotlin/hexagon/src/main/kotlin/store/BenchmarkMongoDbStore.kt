package com.hexagonkt.store

import com.hexagonkt.CachedWorld
import com.hexagonkt.Fortune
import com.hexagonkt.Settings
import com.hexagonkt.World
import com.hexagonkt.helpers.Jvm
import com.hexagonkt.helpers.fail
import com.hexagonkt.store.mongodb.MongoDbStore
import org.cache2k.Cache

internal class BenchmarkMongoDbStore(engine: String, private val settings: Settings = Settings())
    : BenchmarkStore(settings) {

    data class MongoDbWorld(val _id: Int, val id: Int, val randomNumber: Int)
    data class MongoDbFortune(val _id: Int, val message: String)

    private val dbHost: String by lazy { Jvm.systemSetting("${engine.uppercase()}_DB_HOST") ?: "localhost" }

    private val dbUrl: String by lazy { "mongodb://$dbHost/${settings.databaseName}" }

    private val worldRepository: MongoDbStore<MongoDbWorld, Int> by lazy {
        MongoDbStore(MongoDbWorld::class, MongoDbWorld::_id, dbUrl, settings.worldName)
    }

    private val fortuneRepository by lazy {
        MongoDbStore(MongoDbFortune::class, MongoDbFortune::_id, dbUrl, settings.fortuneName)
    }

    override fun findAllFortunes(): List<Fortune> = fortuneRepository.findAll().map { Fortune(it._id, it.message) }

    override fun findWorlds(ids: List<Int>): List<World> =
        ids.mapNotNull { worldRepository.findOne(it) }.map { World(it.id, it.randomNumber) }

    override fun replaceWorlds(worlds: List<World>) {
        worlds.forEach {
            val world = worldRepository.findOne(it.id) ?: fail
            val worldCopy = world.copy(randomNumber = it.randomNumber)
            worldRepository.replaceOne(worldCopy)
        }
    }

    override fun initWorldsCache(cache: Cache<Int, CachedWorld>) {
        worldRepository.findAll().forEach {
            cache.put(it.id, CachedWorld(it.id, it.randomNumber))
        }
    }

    override fun loadCachedWorld(id: Int): CachedWorld =
        worldRepository.findOne(id)
            ?.let { world -> CachedWorld(world.id, world.randomNumber)  }
            ?: error("World not found: $id")

    override fun close() {
        /* Not needed */
    }
}