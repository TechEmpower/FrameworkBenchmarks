package com.hexagonkt

import com.hexagonkt.helpers.systemSetting
import com.hexagonkt.serialization.convertToMap
import com.hexagonkt.serialization.serialize
import com.hexagonkt.server.*
import com.hexagonkt.server.jetty.JettyServletEngine
import com.hexagonkt.server.servlet.ServletServer
import com.hexagonkt.server.undertow.UndertowEngine
import com.hexagonkt.settings.SettingsManager.settings
import com.hexagonkt.templates.pebble.PebbleEngine
import org.slf4j.Logger
import org.slf4j.LoggerFactory.getLogger

import java.util.*
import java.util.concurrent.ThreadLocalRandom
import javax.servlet.annotation.WebListener
import java.net.InetAddress.getByName as address

// DATA CLASSES
internal data class Message(val message: String)
internal data class Fortune(val _id: Int, val message: String)
internal data class World(val _id: Int, val id: Int, val randomNumber: Int)

// CONSTANTS
private const val TEXT_MESSAGE: String = "Hello, World!"
private const val CONTENT_TYPE_JSON = "application/json"
private const val QUERIES_PARAM = "queries"

private val LOGGER: Logger = getLogger("BENCHMARK_LOGGER")
private val defaultLocale: Locale = Locale.getDefault()

// UTILITIES
internal fun randomWorld() = ThreadLocalRandom.current().nextInt(WORLD_ROWS) + 1

private fun Call.returnWorlds(worldsList: List<World>) {
    val worlds = worldsList.map { it.convertToMap() - "_id" }
    ok(worlds.serialize(), CONTENT_TYPE_JSON)
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
    val sortedFortunes = fortunes.sortedBy { it.message }
    response.contentType = "text/html;charset=utf-8"
    template(PebbleEngine, "fortunes.html", defaultLocale, "fortunes" to sortedFortunes)
}

private fun Call.dbQuery(store: Store) {
    val world = store.findWorlds(1).first().convertToMap() - "_id"
    ok(world.serialize(), CONTENT_TYPE_JSON)
}

private fun Call.getWorlds(store: Store) {
    returnWorlds(store.findWorlds(getWorldsCount()))
}

private fun Call.updateWorlds(store: Store) {
    returnWorlds(store.replaceWorlds(getWorldsCount()))
}

// CONTROLLER
private fun router(): Router = router {
    val store = benchmarkStore ?: error("Invalid Store")

    before {
        response.addHeader("Server", "Servlet/3.1")
        response.addHeader("Transfer-Encoding", "chunked")
        response.addHeader("Date", httpDate())
    }

    get("/plaintext") { ok(TEXT_MESSAGE, "text/plain") }
    get("/json") { ok(Message(TEXT_MESSAGE).serialize(), CONTENT_TYPE_JSON) }
    get("/fortunes") { listFortunes(store) }
    get("/db") { dbQuery(store) }
    get("/query") { getWorlds(store) }
    get("/update") { updateWorlds(store) }
}

@WebListener class Web : ServletServer () {
    init {
        if (benchmarkStore == null)
            benchmarkStore = createStore(systemSetting("DBSTORE", "mongodb"))
    }

    override fun createRouter() = router()
}

internal var benchmarkStore: Store? = null
internal var benchmarkServer: Server? = null

internal fun createEngine(engine: String): ServerEngine = when (engine) {
    "jetty" -> JettyServletEngine()
    "undertow" -> UndertowEngine()
    else -> error("Unsupported server engine")
}

fun main(vararg args: String) {
    val engine = createEngine(systemSetting("WEBENGINE", "jetty"))
    benchmarkStore = createStore(systemSetting("DBSTORE", "mongodb"))

    LOGGER.info("""
            Benchmark set up:
                - Engine: {}
                - Store: {}
        """.trimIndent(),
        engine.javaClass.name,
        benchmarkStore?.javaClass?.name)

    benchmarkServer = Server(engine, settings, router()).apply { run() }
}
