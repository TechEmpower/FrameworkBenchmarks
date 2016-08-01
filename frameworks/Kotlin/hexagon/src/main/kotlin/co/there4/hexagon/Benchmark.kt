package co.there4.hexagon

import co.there4.hexagon.settings.SettingsManager.setting
import java.util.concurrent.ThreadLocalRandom

import co.there4.hexagon.serialization.serialize
import co.there4.hexagon.repository.MongoIdRepository
import co.there4.hexagon.repository.mongoCollection
import co.there4.hexagon.repository.mongoDatabase
import co.there4.hexagon.web.*
import co.there4.hexagon.web.jetty.JettyServer

import java.lang.System.getenv
import java.net.InetAddress.getByName as address
import java.time.LocalDateTime.now
import kotlin.reflect.KClass

internal data class Message (val message: String = "Hello, World!")
internal data class Fortune (val _id: Int, val message: String)
internal data class World (val id: Int, val randomNumber: Int)

private val BIND = getenv("OPENSHIFT_DIY_IP") ?: setting<String>("bindAddress") ?: "localhost"
private val BIND_ADDRESS = address(BIND)
private val BIND_PORT = getenv("OPENSHIFT_DIY_PORT")?.toInt() ?: setting<Int>("bindPort") ?: 9090

private val DB_ROWS = 10000
private val CONTENT_TYPE_JSON = "application/json"
private val QUERIES_PARAM = "queries"

private val DB = getenv("OPENSHIFT_APP_NAME") ?: setting<String>("database") ?: "hello_world"
private val WORLD: String = setting<String>("worldCollection") ?: "world"
private val FORTUNE: String = setting<String>("fortuneCollection") ?: "fortune"

private val DB_HOST = getenv("DBHOST") ?: "localhost"
private val DB_PORT = getenv("OPENSHIFT_MONGODB_DB_PORT") ?: 27017
private val DB_USER = getenv("OPENSHIFT_MONGODB_DB_USERNAME")
private val DB_PASS = getenv("OPENSHIFT_MONGODB_DB_PASSWORD")

private val database =
    if (DB_USER == null) mongoDatabase("mongodb://$DB_HOST:$DB_PORT/$DB")
    else mongoDatabase("mongodb://$DB_USER:$DB_PASS@$DB_HOST:$DB_PORT/$DB")

private val worldRepository = repository(World::class, WORLD, { it.id })
private val fortuneRepository = repository(Fortune::class, FORTUNE, { it._id })

private fun <T : Any> repository(type: KClass<T>, name: String, keySupplier: (T) -> Int) =
    MongoIdRepository(type, mongoCollection(name, database), keySupplier, Int::class, "_id")

private fun rnd () = ThreadLocalRandom.current ().nextInt (DB_ROWS) + 1

private fun Exchange.hasQueryCount() = request[QUERIES_PARAM] == null

private fun Exchange.getDb () {
    val worlds = (1..getQueries()).map { worldRepository.find(rnd ()) }

    response.contentType = CONTENT_TYPE_JSON
    ok (if (hasQueryCount()) worlds[0].serialize() else worlds.serialize())
}

private fun Exchange.getFortunes () {
    val fortune = Fortune (0, "Additional fortune added at request time.")
    val fortunes = fortuneRepository.findObjects ().toList() + fortune

    response.contentType = "text/html; charset=utf-8"
    template ("fortunes.html", mapOf ("fortunes" to fortunes.sortedBy { it.message }))
}

private fun Exchange.getUpdates () {
    val worlds =  (1..getQueries()).map {
        val newWorld = World (rnd (), rnd())
        worldRepository.replaceObject (newWorld)
        newWorld
    }

    response.contentType = CONTENT_TYPE_JSON
    ok (if (hasQueryCount()) worlds[0].serialize() else worlds.serialize())
}

private fun Exchange.getQueries () =
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

private fun Exchange.getPlaintext () {
    response.contentType = "text/plain"
    ok("Hello, World!")
}

private fun Exchange.getJson () {
    response.contentType = CONTENT_TYPE_JSON
    ok(Message ().serialize())
}

fun main(args: Array<String>) {
    server = JettyServer (bindAddress = BIND_ADDRESS, bindPort = BIND_PORT)

    before {
        response.addHeader("Server", "Servlet/3.1")
        response.addHeader("Transfer-Encoding", "chunked")
        response.addHeader("Date", httpDate (now()))
    }

    get ("/json") { getJson() }
    get ("/db") { getDb() }
    get ("/query") { getDb() }
    get ("/fortune") { getFortunes() }
    get ("/update") { getUpdates() }
    get ("/plaintext") { getPlaintext() }

    run ()
}
