package com.hexagonkt

import com.hexagonkt.helpers.Jvm.systemSetting
import com.hexagonkt.serialization.Json
import com.hexagonkt.serialization.convertToMap
import com.hexagonkt.http.server.*
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.http.server.servlet.ServletServer
import com.hexagonkt.settings.SettingsManager
import com.hexagonkt.templates.TemplateManager.render
import com.hexagonkt.templates.TemplatePort
import com.hexagonkt.templates.pebble.PebbleAdapter

import java.util.Locale

import javax.servlet.annotation.WebListener

// DATA CLASSES
internal data class Message(val message: String)
internal data class Fortune(val _id: Int, val message: String)
internal data class World(val _id: Int, val id: Int, val randomNumber: Int)

// CONSTANTS
private const val TEXT_MESSAGE: String = "Hello, World!"
private const val QUERIES_PARAM: String = "queries"

internal val benchmarkStores: Map<String, BenchmarkStore> by lazy {
    mapOf(
        "mongodb" to BenchmarkMongoDbStore("mongodb"),
        "postgresql" to BenchmarkSqlStore("postgresql")
    )
}

internal val benchmarkTemplateEngines: Map<String, TemplatePort> by lazy {
    mapOf("pebble" to PebbleAdapter)
}

private val defaultLocale = Locale.getDefault()

private val engine by lazy {
    when (systemSetting("WEBENGINE", "jetty")) {
        "jetty" -> JettyServletAdapter()
        else -> error("Unsupported server engine")
    }
}

private val router: Router by lazy {
    Router {
        before {
            response.setHeader("Server", "Servlet/3.1")
            response.setHeader("Transfer-Encoding", "chunked")
        }

        get("/plaintext") { ok(TEXT_MESSAGE, "text/plain") }
        get("/json") { ok(Message(TEXT_MESSAGE), Json) }

        benchmarkStores.forEach { storeEngine, store ->
            benchmarkTemplateEngines.forEach { templateKind ->
                val path = "/$storeEngine/${templateKind.key}/fortunes"

                get(path) { listFortunes(store, templateKind.key, templateKind.value) }
            }

            get("/$storeEngine/db") { dbQuery(store) }
            get("/$storeEngine/query") { getWorlds(store) }
            get("/$storeEngine/update") { updateWorlds(store) }
        }
    }
}

internal val benchmarkServer: Server by lazy { Server(engine, router, SettingsManager.settings) }

// UTILITIES
private fun returnWorlds(worldsList: List<World>): List<Map<Any?, Any?>> =
    worldsList.map { it.convertToMap() - "_id" }

private fun Call.getWorldsCount() = parameters[QUERIES_PARAM]?.firstOrNull()?.toIntOrNull().let {
    when {
        it == null -> 1
        it < 1 -> 1
        it > 500 -> 500
        else -> it
    }
}

// HANDLERS
private fun Call.listFortunes(
    store: BenchmarkStore, templateKind: String, templateAdapter: TemplatePort) {

    val fortunes = store.findAllFortunes() + Fortune(0, "Additional fortune added at request time.")
    val sortedFortunes = fortunes.sortedBy { it.message }
    val context = mapOf("fortunes" to sortedFortunes)

    response.contentType = "text/html;charset=utf-8"
    ok(render(templateAdapter, "fortunes.$templateKind.html", defaultLocale, context))
}

private fun Call.dbQuery(store: BenchmarkStore) {
    ok(returnWorlds(store.findWorlds(1)).first(), Json)
}

private fun Call.getWorlds(store: BenchmarkStore) {
    ok(returnWorlds(store.findWorlds(getWorldsCount())), Json)
}

private fun Call.updateWorlds(store: BenchmarkStore) {
    ok(returnWorlds(store.replaceWorlds(getWorldsCount())), Json)
}

// SERVERS
@WebListener class Web : ServletServer(router)

fun main() {
    benchmarkServer.run()
}
