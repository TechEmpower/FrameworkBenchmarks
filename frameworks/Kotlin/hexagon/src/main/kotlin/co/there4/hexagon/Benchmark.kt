package co.there4.hexagon

import java.util.*
import java.util.concurrent.ThreadLocalRandom
import kotlin.reflect.KClass

import co.there4.hexagon.ratpack.KContext
import co.there4.hexagon.rest.applicationStart
import co.there4.hexagon.serialization.serialize
import co.there4.hexagon.repository.MongoIdRepository
import co.there4.hexagon.repository.mongoCollection
import co.there4.hexagon.repository.mongoDatabase

import ratpack.server.BaseDir

import co.there4.hexagon.Benchmark.Companion.DB_ROWS
import java.lang.System.getenv
import java.net.InetAddress
import java.time.LocalDateTime

/*
 * TODO Use framework ConfigManager
 */
internal class MongoDbRepository (settings: Properties) {
    val DATABASE = settings.getProperty ("mongodb.database")
    val WORLD = settings.getProperty ("mongodb.world.collection")
    val FORTUNE = settings.getProperty ("mongodb.fortune.collection")

    val database = mongoDatabase("mongodb://${getenv("DBHOST") ?: "localhost"}/$DATABASE")
    val worldRepository = idRepository(World::class, WORLD, Int::class, { it.id })
    val fortuneRepository = idRepository(Fortune::class, FORTUNE, Int::class, { it.id })

    val random = ThreadLocalRandom.current ()

    fun <T : Any, K : Any> idRepository (
        type: KClass<T>, collectionName: String, keyType: KClass<K>, keySupplier: (T) -> K) =
            MongoIdRepository(
                type,
                mongoCollection (collectionName, database),
                "_id",
                keyType,
                keySupplier
            )

    fun rnd () = random.nextInt (DB_ROWS) + 1

    fun getWorlds (queries: Int, update: Boolean) = (1..queries).map {
        if (update) {
            val newWorld = World (rnd (), rnd())
            worldRepository.replaceObject (newWorld)
            newWorld
        }
        else {
            worldRepository.find(rnd ())
        }
    }
}

internal data class Message (val message: String = "Hello, World!")
internal data class Fortune (val id: Int, val message: String)
internal data class World (val id: Int, val randomNumber: Int)

internal class Benchmark {
    companion object {
        val SETTINGS_RESOURCE = "/benchmark.properties"
        val DB_ROWS = 10000

        val MESSAGE = "Hello, World!"
        val CONTENT_TYPE_TEXT = "text/plain"
        val CONTENT_TYPE_JSON = "application/json"
        val QUERIES_PARAM = "queries"

        val repository = MongoDbRepository (loadConfiguration ())

        fun loadConfiguration (): Properties {
            val settings = Properties ()
            settings.load (Benchmark::class.java.getResourceAsStream (SETTINGS_RESOURCE))
            return settings
        }
    }

    private fun KContext.handle (callback: KContext.() -> Unit) {
        try {
            callback()
        }
        catch (e: Exception) {
            halt (e.message ?: "")
        }
    }

    private fun KContext.getDb () {
        handle {
            val worlds = repository.getWorlds (getQueries (), false)
            response.contentType (CONTENT_TYPE_JSON)
            ok (
                if (request.queryParams [QUERIES_PARAM] == null) worlds[0].serialize()
                else worlds.serialize()
            )
        }
    }

    private fun KContext.getFortunes () {
        handle {
            val fortune = Fortune (0, "Additional fortune added at request time.")
            val fortunes:List<Fortune> = repository.fortuneRepository.findObjects ().toList() + fortune
            fortunes.sortedBy { it.message }

            response.contentType ("text/html; charset=utf-8")
            template ("fortunes.html", mapOf ("fortunes" to fortunes))
        }
    }

    private fun KContext.getUpdates () {
        handle {
            val worlds = repository.getWorlds (getQueries (), true)
            response.contentType (CONTENT_TYPE_JSON)
            ok (
                if (request.queryParams [QUERIES_PARAM] == null) worlds[0].serialize()
                else worlds.serialize()
            )
        }
    }

    private fun KContext.getQueries (): Int {
        try {
            val parameter = request.queryParams [QUERIES_PARAM] ?: return 1

            val queries = parameter.toInt()
            if (queries < 1)
                return 1
            if (queries > 500)
                return 500

            return queries
        }
        catch (ex: NumberFormatException) {
            return 1
        }
    }

    private fun KContext.getPlaintext () {
        response.contentType (CONTENT_TYPE_TEXT)
        ok(MESSAGE)
    }

    private fun KContext.getJson () {
        response.contentType (CONTENT_TYPE_JSON)
        ok(Message ().serialize())
    }

    init {
        applicationStart {
            serverConfig {
                val settings = loadConfiguration ()
                port(settings.getProperty ("web.port").toInt())
                address(InetAddress.getByName(settings.getProperty ("web.host")))
                baseDir(BaseDir.find("benchmark.properties"))
                development(false)
            }

            handlers {
                all {
                    response.headers ["Server"] = "Ratpack/1.3"
                    response.headers ["Transfer-Encoding"] = "chunked"
                    response.headers ["Date"] = httpDate (LocalDateTime.now())
                    next()
                }
                get ("json") { getJson() }
                get ("db") { getDb() }
                get ("query") { getDb() }
                get ("fortune") { getFortunes() }
                get ("update") { getUpdates() }
                get ("plaintext") { getPlaintext() }
            }
        }
    }
}

fun main(args: Array<String>) {
    Benchmark()
}
