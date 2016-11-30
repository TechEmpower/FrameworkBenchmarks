package co.there4.hexagon

import co.there4.hexagon.repository.*
import co.there4.hexagon.serialization.serialize
import co.there4.hexagon.settings.SettingsManager.setting
import co.there4.hexagon.web.*

import java.lang.System.getenv
import java.net.InetAddress.getByName as address
import java.time.LocalDateTime.now
import java.util.concurrent.ThreadLocalRandom

import kotlin.reflect.KProperty1

internal data class Message(val message: String = "Hello, World!")
internal data class Fortune(val _id: Int, val message: String)
internal data class World(val _id: Int, val id: Int, val randomNumber: Int)

private val DB_ROWS = 10000
private val CONTENT_TYPE_JSON = "application/json"
private val QUERIES_PARAM = "queries"

private val DB_HOST = getenv("DBHOST") ?: "localhost"
private val DB = setting<String>("database") ?: "hello_world"
private val WORLD: String = setting<String>("worldCollection") ?: "world"
private val FORTUNE: String = setting<String>("fortuneCollection") ?: "fortune"

private val database = mongoDatabase("mongodb://$DB_HOST/$DB")
private val worldRepository = repository(WORLD, World::_id)
private val fortuneRepository = repository(FORTUNE, Fortune::_id)

private inline fun <reified T : Any> repository(name: String, key: KProperty1<T, Int>) =
    MongoIdRepository(T::class, mongoCollection(name, database), key)

private fun rnd() = ThreadLocalRandom.current().nextInt(DB_ROWS) + 1

private fun Exchange.hasQueryCount() = request[QUERIES_PARAM] == null

private fun Exchange.getDb() {
    val worlds = (1..getQueries()).map { worldRepository.find(rnd()) }.filterNotNull()

    response.contentType = CONTENT_TYPE_JSON
    ok(if (hasQueryCount()) worlds[0].serialize() else worlds.serialize())
}

private fun Exchange.getFortunes() {
    val fortune = Fortune(0, "Additional fortune added at request time.")
    val fortunes = fortuneRepository.findObjects().toList() + fortune

    template("fortunes.html", mapOf("fortunes" to fortunes.sortedBy { it.message }))
}

private fun Exchange.getUpdates() {
    val worlds = (1..getQueries()).map {
        val id = rnd()
        val newWorld = World(id, id, rnd())
        worldRepository.replaceObject(newWorld)
        newWorld
    }

    response.contentType = CONTENT_TYPE_JSON
    ok(if (hasQueryCount()) worlds[0].serialize() else worlds.serialize())
}

private fun Exchange.getQueries() =
    try {
        val queries = request[QUERIES_PARAM]?.toInt() ?: 1
        when {
            queries < 1 -> 1
            queries > 500 -> 500
            else -> queries
        }
    }
    catch (ex: NumberFormatException) {
        1
    }

private fun Exchange.getPlaintext() {
    response.contentType = "text/plain"
    ok("Hello, World!")
}

private fun Exchange.getJson() {
    response.contentType = CONTENT_TYPE_JSON
    ok(Message().serialize())
}

fun main(args: Array<String>) {
    before {
        response.addHeader("Server", "Servlet/3.1")
        response.addHeader("Transfer-Encoding", "chunked")
        response.addHeader("Date", httpDate(now()))
    }

    get("/json") { getJson() }
    get("/db") { getDb() }
    get("/query") { getDb() }
    get("/fortune") { getFortunes() }
    get("/update") { getUpdates() }
    get("/plaintext") { getPlaintext() }

    run()
}
