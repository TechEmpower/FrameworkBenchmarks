package com.hexagontk

import com.hexagontk.core.fieldsMapOf
import com.hexagontk.core.media.APPLICATION_JSON
import com.hexagontk.core.media.TEXT_HTML
import com.hexagontk.core.media.TEXT_PLAIN
import com.hexagontk.http.model.ContentType
import com.hexagontk.http.model.Field
import com.hexagontk.http.model.Headers
import com.hexagontk.http.server.callbacks.DateCallback
import com.hexagontk.http.handlers.HttpContext
import com.hexagontk.http.handlers.PathHandler
import com.hexagontk.http.handlers.path
import com.hexagontk.model.*
import com.hexagontk.serialization.jackson.json.Json
import com.hexagontk.serialization.serialize
import com.hexagontk.store.BenchmarkStore
import com.hexagontk.templates.TemplatePort
import java.net.URL
import java.util.concurrent.ThreadLocalRandom
import kotlin.text.Charsets.UTF_8

class Controller(
    settings: Settings,
    store: BenchmarkStore,
    templateEngine: TemplatePort,
    templateUrl: URL,
) {
    private val queriesParam: String = settings.queriesParam
    private val cachedQueriesParam: String = settings.cachedQueriesParam
    private val worldRows: Int = settings.worldRows
    private val textMessage: String = settings.textMessage

    private val plain: ContentType = ContentType(TEXT_PLAIN)
    private val json: ContentType = ContentType(APPLICATION_JSON)
    private val html: ContentType = ContentType(TEXT_HTML, charset = UTF_8)

    private val headers = Headers(Field("server", "Hexagon"))

    val path: PathHandler by lazy {
        path {
            before("*") { send(headers = headers) }
            before("*", DateCallback())

            get("/plaintext") { ok(textMessage, contentType = plain) }
            get("/json") { ok(Message(textMessage).toJson(), contentType = json) }
            get("/fortunes") { listFortunes(store, templateUrl, templateEngine) }
            get("/db") { dbQuery(store) }
            get("/query") { getWorlds(store) }
            get("/cached-queries") { getCachedWorlds(store) }
            get("/update") { updateWorlds(store) }
        }
    }

    private fun Message.toJson(): String =
        toMap().serialize(Json.raw)

    private fun HttpContext.listFortunes(
        store: BenchmarkStore, templateUrl: URL, templateAdapter: TemplatePort
    ): HttpContext {

        val fortunes = store.findAllFortunes() + Fortune(0, "Additional fortune added at request time.")
        val sortedFortunes = fortunes.sortedBy { it.message }
        val context = mapOf("fortunes" to sortedFortunes)
        // TODO Pass the map with the template
        val body = templateAdapter.render(templateUrl, context)

        return ok(body, contentType = html)
    }

    private fun HttpContext.dbQuery(store: BenchmarkStore): HttpContext {
        val ids = listOf(randomWorld())
        val worlds = store.findWorlds(ids)
        val world = worlds.first().toMap()

        return sendJson(world)
    }

    private fun HttpContext.getWorlds(store: BenchmarkStore): HttpContext {
        val worldsCount = getWorldsCount(queriesParam)
        val ids = (1..worldsCount).map { randomWorld() }
        val worlds = store.findWorlds(ids).map { it.toMap() }

        return sendJson(worlds)
    }

    private fun HttpContext.getCachedWorlds(store: BenchmarkStore): HttpContext {
        val worldsCount = getWorldsCount(cachedQueriesParam)
        val ids = (1..worldsCount).map { randomWorld() }
        val worlds = store.findCachedWorlds(ids).map { it.toMap() }

        return sendJson(worlds)
    }

    private fun HttpContext.updateWorlds(store: BenchmarkStore): HttpContext {
        val worldsCount = getWorldsCount(queriesParam)
        val worlds = (1..worldsCount).map { World(randomWorld(), randomWorld()) }

        store.replaceWorlds(worlds)

        return sendJson(worlds.map { it.toMap() })
    }

    private fun HttpContext.sendJson(body: Any): HttpContext =
        ok(body.serialize(Json.raw), contentType = json)

    private fun HttpContext.getWorldsCount(parameter: String): Int =
        request.queryParameters[parameter]?.text?.toIntOrNull().let {
            when {
                it == null -> 1
                it < 1 -> 1
                it > 500 -> 500
                else -> it
            }
        }

    private fun randomWorld(): Int =
        ThreadLocalRandom.current().nextInt(worldRows) + 1

    private fun Message.toMap(): Map<String, *> =
        fieldsMapOf(Message::message to message)

    private fun World.toMap(): Map<String, *> =
        fieldsMapOf(
            World::id to id,
            World::randomNumber to randomNumber,
        )

    private fun CachedWorld.toMap(): Map<String, *> =
        fieldsMapOf(
            CachedWorld::id to id,
            CachedWorld::randomNumber to randomNumber,
        )
}
