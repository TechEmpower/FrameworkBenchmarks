package com.hexagonkt

import com.hexagonkt.http.server.Call
import com.hexagonkt.http.server.Router
import com.hexagonkt.serialization.Json
import com.hexagonkt.serialization.toFieldsMap
import com.hexagonkt.store.BenchmarkStore
import com.hexagonkt.templates.TemplatePort
import java.util.concurrent.ThreadLocalRandom

class Controller(private val settings: Settings) {

    internal val router: Router by lazy {
        Router {
            before {
                response.headers["Server"] = "Servlet/3.1"
                response.headers["Transfer-Encoding"] = "chunked"
            }

            get("/plaintext") { ok(settings.textMessage, "text/plain") }
            get("/json") { ok(Message(settings.textMessage), Json) }

            benchmarkStores.forEach { (storeEngine, store) ->
                benchmarkTemplateEngines.forEach { templateKind ->
                    val path = "/$storeEngine/${templateKind.key}/fortunes"

                    get(path) { listFortunes(store, templateKind.key, templateKind.value) }
                }

                get("/$storeEngine/db") { dbQuery(store) }
                get("/$storeEngine/query") { getWorlds(store) }
                get("/$storeEngine/cached") { getCachedWorlds(store) }
                get("/$storeEngine/update") { updateWorlds(store) }
            }
        }
    }

    private fun Call.listFortunes(store: BenchmarkStore, templateKind: String, templateAdapter: TemplatePort) {

        val fortunes = store.findAllFortunes() + Fortune(0, "Additional fortune added at request time.")
        val sortedFortunes = fortunes.sortedBy { it.message }
        val context = mapOf("fortunes" to sortedFortunes)

        response.contentType = "text/html;charset=utf-8"
        ok(templateAdapter.render("fortunes.$templateKind.html", context))
    }

    private fun Call.dbQuery(store: BenchmarkStore) {
        ok(store.findWorlds(listOf(randomWorld())).first(), Json)
    }

    private fun Call.getWorlds(store: BenchmarkStore) {
        val ids = (1..getWorldsCount(settings.queriesParam)).map { randomWorld() }
        ok(store.findWorlds(ids), Json)
    }

    private fun Call.getCachedWorlds(store: BenchmarkStore) {
        val ids = (1..getWorldsCount(settings.cachedQueriesParam)).map { randomWorld() }
        ok(store.findCachedWorlds(ids).map { it.toFieldsMap() }, Json)
    }

    private fun Call.updateWorlds(store: BenchmarkStore) {
        val worlds = (1..getWorldsCount(settings.queriesParam)).map { World(randomWorld(), randomWorld()) }
        store.replaceWorlds(worlds)
        ok(worlds, Json)
    }

    private fun Call.getWorldsCount(parameter: String): Int =
        queryParametersValues[parameter]?.firstOrNull()?.toIntOrNull().let {
            when {
                it == null -> 1
                it < 1 -> 1
                it > 500 -> 500
                else -> it
            }
        }

    private fun randomWorld(): Int =
        ThreadLocalRandom.current().nextInt(settings.worldRows) + 1
}