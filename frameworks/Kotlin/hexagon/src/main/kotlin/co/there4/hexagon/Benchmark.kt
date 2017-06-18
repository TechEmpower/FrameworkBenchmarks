package co.there4.hexagon

import co.there4.hexagon.serialization.convertToMap
import co.there4.hexagon.serialization.serialize
import co.there4.hexagon.server.*
import co.there4.hexagon.server.engine.servlet.JettyServletEngine
import co.there4.hexagon.server.engine.servlet.ServletServer
import co.there4.hexagon.settings.SettingsManager.settings
import java.lang.System.getenv

import java.net.InetAddress.getByName as address
import java.util.concurrent.ThreadLocalRandom
import javax.servlet.annotation.WebListener

// DATA CLASSES
internal data class Message(val message: String)
internal data class Fortune(val _id: Int, val message: String)
internal data class World(val _id: Int, val id: Int, val randomNumber: Int)

// CONSTANTS
private const val TEXT_MESSAGE: String = "Hello, World!"
private const val CONTENT_TYPE_JSON = "application/json"
private const val QUERIES_PARAM = "queries"

internal var server: Server? = null

// UTILITIES
internal fun randomWorld() = ThreadLocalRandom.current().nextInt(WORLD_ROWS) + 1

private fun Call.returnWorlds(worldsList: List<World>) {
    val worlds = worldsList.map { it.convertToMap() - "_id" }
    val result = if (worlds.size == 1) worlds.first().serialize() else worlds.serialize()

    ok(result, CONTENT_TYPE_JSON)
}

private fun Call.getWorldsCount() = (request[QUERIES_PARAM]?.toIntOrNull() ?: 1).let {
    when {
        it < 1 -> 1
        it > 500 -> 500
        else -> it
    }
}

// HANDLERS
private fun Call.listFortunes(store: Store) {
    val fortunes = store.findAllFortunes() + Fortune(0, "Additional fortune added at request time.")
    response.contentType = "text/html; charset=utf-8"
    template("fortunes.html", "fortunes" to fortunes.sortedBy { it.message })
}

private fun Call.getWorlds(store: Store) {
    returnWorlds(store.findWorlds(getWorldsCount()))
}

private fun Call.updateWorlds(store: Store) {
    returnWorlds(store.replaceWorlds(getWorldsCount()))
}

private fun router(store: Store): Router = router {
    before {
        response.addHeader("Server", "Servlet/3.1")
        response.addHeader("Transfer-Encoding", "chunked")
        response.addHeader("Date", httpDate())
    }

    get("/plaintext") { ok(TEXT_MESSAGE, "text/plain") }
    get("/json") { ok(Message(TEXT_MESSAGE).serialize(), CONTENT_TYPE_JSON) }
    get("/fortunes") { listFortunes(store) }
    get("/db") { getWorlds(store) }
    get("/query") { getWorlds(store) }
    get("/update") { updateWorlds(store) }
}

@WebListener class Web : ServletServer () {
    override fun createRouter() = router (createStore(getenv("DBSTORE") ?: "mongodb"))
}

fun main(vararg args: String) {
    val store = createStore(if (args.isEmpty()) getenv("DBSTORE") ?: "mongodb" else args[0])
    server = Server(JettyServletEngine(), settings, router(store))
    server?.run()
}
