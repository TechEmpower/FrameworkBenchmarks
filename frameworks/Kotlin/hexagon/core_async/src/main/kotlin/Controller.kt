package com.hexagonkt.async

import com.hexagonkt.Settings
import com.hexagonkt.core.fieldsMapOf
import com.hexagonkt.core.media.APPLICATION_JSON
import com.hexagonkt.core.media.TEXT_HTML
import com.hexagonkt.core.media.TEXT_PLAIN
import com.hexagonkt.handlers.async.done
import com.hexagonkt.http.model.ContentType
import com.hexagonkt.http.model.Header
import com.hexagonkt.http.model.Headers
import com.hexagonkt.http.server.async.callbacks.DateCallback
import com.hexagonkt.http.handlers.async.HttpContext
import com.hexagonkt.http.handlers.async.PathHandler
import com.hexagonkt.http.handlers.async.path
import com.hexagonkt.model.*
import com.hexagonkt.serialization.jackson.json.Json
import com.hexagonkt.serialization.serialize
import com.hexagonkt.async.store.BenchmarkStore
import com.hexagonkt.templates.TemplatePort
import java.net.URL
import java.util.concurrent.CompletableFuture
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

    private val headers = Headers(Header("server", "Hexagon"))

    val path: PathHandler by lazy {
        path {
            on("*") { send(headers = headers).done() }
            on("*", DateCallback())

            get("/plaintext") { ok(textMessage, contentType = plain).done() }
            get("/json") { ok(Message(textMessage).toJson(), contentType = json).done() }
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
    ): CompletableFuture<HttpContext> =
        store
            .findAllFortunes()
            .thenApply { it + Fortune(0, "Additional fortune added at request time.")  }
            .thenApply { fortunes -> fortunes.sortedBy { it.message } }
            .thenApply { sortedFortunes -> mapOf("fortunes" to sortedFortunes) }
            .thenApply { context -> templateAdapter.render(templateUrl, context) }
            .thenApply { body -> ok(body, contentType = html) }

    private fun HttpContext.dbQuery(store: BenchmarkStore): CompletableFuture<HttpContext> {
        val ids = listOf(randomWorld())
        return store.findWorlds(ids)
            .thenApply { worlds -> worlds.first().toMap()}
            .thenApply { world -> sendJson(world) }
    }

    private fun HttpContext.getWorlds(store: BenchmarkStore): CompletableFuture<HttpContext> {
        val worldsCount = getWorldsCount(queriesParam)
        val ids = (1..worldsCount).map { randomWorld() }
        return store.findWorlds(ids)
            .thenApply { worlds -> worlds.map { it.toMap() } }
            .thenApply { worlds -> sendJson(worlds) }
    }

    private fun HttpContext.getCachedWorlds(store: BenchmarkStore): CompletableFuture<HttpContext> {
        val worldsCount = getWorldsCount(cachedQueriesParam)
        val ids = (1..worldsCount).map { randomWorld() }
        val worlds = store.findCachedWorlds(ids).map { it.toMap() }

        return sendJson(worlds).done()
    }

    private fun HttpContext.updateWorlds(store: BenchmarkStore): CompletableFuture<HttpContext> {
        val worldsCount = getWorldsCount(queriesParam)
        val worlds = (1..worldsCount).map { World(randomWorld(), randomWorld()) }

        return store.replaceWorlds(worlds)
            .thenApply { sendJson(worlds.map { it.toMap() }) }
    }

    private fun HttpContext.sendJson(body: Any): HttpContext =
        ok(body.serialize(Json.raw), contentType = json)

    private fun HttpContext.getWorldsCount(parameter: String): Int =
        request.queryParameters[parameter]?.string()?.toIntOrNull().let {
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
