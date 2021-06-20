package com.hexagonkt.store

import com.hexagonkt.Fortune
import com.hexagonkt.Settings
import com.hexagonkt.World
import com.hexagonkt.helpers.Jvm
import com.hexagonkt.helpers.fail
import com.hexagonkt.store.mongodb.MongoDbStore

internal class BenchmarkMongoDbStore(engine: String, private val settings: Settings = Settings()) : BenchmarkStore {

    private val dbHost: String by lazy { Jvm.systemSetting("${engine.uppercase()}_DB_HOST") ?: "localhost" }

    private val dbUrl: String by lazy { "mongodb://$dbHost/${settings.databaseName}" }

    private val worldRepository by lazy {
        MongoDbStore(World::class, World::_id, dbUrl, settings.worldName)
    }

    private val fortuneRepository by lazy {
        MongoDbStore(Fortune::class, Fortune::_id, dbUrl, settings.fortuneName)
    }

    override fun findAllFortunes(): List<Fortune> = fortuneRepository.findAll()

    override fun findWorlds(count: Int): List<World> =
        (1..count).mapNotNull { worldRepository.findOne(randomWorld(settings.worldRows)) }

    override fun replaceWorlds(count: Int): List<World> = (1..count)
        .map {
            val world = worldRepository.findOne(randomWorld(settings.worldRows)) ?: fail
            val worldCopy = world.copy(randomNumber = randomWorld(settings.worldRows))
            worldRepository.replaceOne(worldCopy)
            worldCopy
        }

    override fun close() {
        /* Not needed */
    }
}