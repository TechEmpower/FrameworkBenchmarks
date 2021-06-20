package com.hexagonkt

import com.hexagonkt.http.server.Call
import com.hexagonkt.http.server.Router
import com.hexagonkt.serialization.Json
import com.hexagonkt.serialization.toFieldsMap
import com.hexagonkt.store.BenchmarkStore
import com.hexagonkt.templates.TemplateManager
import com.hexagonkt.templates.TemplatePort
import java.util.*

class Controller(private val settings: Settings) {

    private val defaultLocale = Locale.getDefault()

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
                get("/$storeEngine/update") { updateWorlds(store) }
            }
        }
    }

    private fun returnWorlds(worldsList: List<World>): List<Map<Any?, Any?>> =
        worldsList.map { it.toFieldsMap() - "_id" }

    private fun Call.getWorldsCount(): Int =
        queryParametersValues[settings.queriesParam]?.firstOrNull()?.toIntOrNull().let {
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
        ok(TemplateManager.render(templateAdapter, "fortunes.$templateKind.html", defaultLocale, context))
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
}