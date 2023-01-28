import io.vertx.core.CompositeFuture
import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServer
import io.vertx.core.http.HttpServerRequest
import io.vertx.core.http.HttpServerResponse
import io.vertx.ext.web.Route
import io.vertx.ext.web.Router
import io.vertx.ext.web.RoutingContext
import io.vertx.kotlin.core.http.httpServerOptionsOf
import io.vertx.kotlin.coroutines.CoroutineVerticle
import io.vertx.kotlin.coroutines.await
import io.vertx.kotlin.pgclient.pgConnectOptionsOf
import io.vertx.kotlin.sqlclient.poolOptionsOf
import io.vertx.pgclient.PgPool
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.RowSet
import io.vertx.sqlclient.Tuple
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.launch
import kotlinx.html.*
import kotlinx.html.stream.createHTML
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

class MainVerticle : CoroutineVerticle() {
    inline fun Route.checkedCoroutineHandler(crossinline requestHandler: suspend (RoutingContext) -> Unit): Route =
        handler { ctx ->
            // TODO: is using `Dispatchers.Unconfined` faster?
            launch(Dispatchers.Unconfined) {
                try {
                    requestHandler(ctx)
                } catch (t: Throwable) {
                    ctx.fail(t)
                }
            }
        }

    lateinit var pgPool: PgPool
    lateinit var date: String
    lateinit var httpServer: HttpServer

    fun setCurrentDate() {
        // kotlinx-datetime doesn't support the format yet
        //date = Clock.System.now().toString()
        date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now())
    }

    override suspend fun start() {
        // TODO: tune the parameters
        pgPool = PgPool.pool(
            vertx,
            pgConnectOptionsOf(
                database = "hello_world",
                host = "tfb-database",
                user = "benchmarkdbuser",
                password = "benchmarkdbpass",
            ), poolOptionsOf()
        )
        setCurrentDate()
        vertx.setPeriodic(1000) { setCurrentDate() }
        httpServer = vertx.createHttpServer(httpServerOptionsOf(port = 8080))
            .requestHandler(Router.router(vertx).apply { routes() })
            .exceptionHandler { it.printStackTrace() }
            .listen().await()
    }


    fun HttpServerRequest.getQueries(): Int {
        //it.queryParam("queries") // TODO: is this faster?
        val queriesParam: String? = getParam("queries")
        return queriesParam?.toIntOrNull()?.coerceIn(1, 500) ?: 1
    }

    @Suppress("NOTHING_TO_INLINE")
    inline fun HttpServerResponse.putCommonHeaders() {
        putHeader(HttpHeaders.SERVER, "Vert.x Web")
        putHeader(HttpHeaders.DATE, date)
    }

    @Suppress("NOTHING_TO_INLINE")
    inline fun HttpServerResponse.putJsonResponseHeader() {
        putCommonHeaders()
        putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
    }

    inline fun <reified T : Any> Route.jsonResponseHandler(crossinline requestHandler: suspend (RoutingContext) -> @Serializable T) =
        checkedCoroutineHandler {
            it.response().run {
                putJsonResponseHeader()
                // TODO: which is faster? Vert.x or kotlinx.serialization?
                end(Json.encodeToString(requestHandler(it))).await()
            }
        }

    // batch execution which seems not permitted
    // TODO: how much faster?
    suspend fun batchSelectRandomWorlds(queries: Int): List<World> =
        pgPool.preparedQuery(SELECT_WORLD_SQL)
            .executeBatch(List(queries) { Tuple.of(randomIntBetween1And10000()) }).await()
            .map { it.toWorld() }

    suspend fun selectRandomWorlds(queries: Int): List<World> {
        val rowSets = CompositeFuture.all(List(queries) {
            pgPool.preparedQuery(SELECT_WORLD_SQL).execute(Tuple.of(randomIntBetween1And10000()))
        }).await().list<RowSet<Row>>()
        return rowSets.map { it.single().toWorld() }
    }

    // TODO: is this approach faster?
    // an alternative approach
    suspend fun selectRandomWorlds2(queries: Int): List<World> =
        List(queries) {
            async {
                val rowSet = pgPool.preparedQuery(SELECT_WORLD_SQL)
                    .execute(Tuple.of(randomIntBetween1And10000())).await()
                rowSet.single().toWorld()
            }
        }.awaitAll()

    fun Router.routes() {
        get("/json").jsonResponseHandler {
            jsonSerializationMessage
        }

        get("/db").jsonResponseHandler {
            val rowSet = pgPool.preparedQuery(SELECT_WORLD_SQL).execute(Tuple.of(randomIntBetween1And10000())).await()
            // TODO: how much is using `first()` faster?
            rowSet.single().toWorld()
        }

        get("/queries").jsonResponseHandler {
            val queries = it.request().getQueries()
            selectRandomWorlds(queries)
        }

        get("/fortunes").checkedCoroutineHandler {
            val fortunes = mutableListOf<Fortune>()
            pgPool.preparedQuery(SELECT_FORTUNE_SQL).execute().await()
                .mapTo(fortunes) { it.toFortune() }

            fortunes.add(Fortune(0, "Additional fortune added at request time."))
            fortunes.sortBy { it.message }

            // TODO: is using `buildString` faster? Refer to https://github.com/Kotlin/kotlinx.html/wiki/Streaming.
            val htmlString = "<!DOCTYPE html>" + createHTML(false).html {
                head {
                    title("Fortunes")
                }
                body {
                    table {
                        tr {
                            th { +"id" }
                            th { +"message" }
                        }
                        for (fortune in fortunes)
                            tr {
                                td { +fortune.id.toString() }
                                td { +fortune.message }
                            }
                    }
                }
            }

            it.response().run {
                putCommonHeaders()
                putHeader(HttpHeaders.CONTENT_TYPE, "text/html; charset=utf-8")
                end(htmlString).await()
            }
        }

        get("/updates").jsonResponseHandler {
            val queries = it.request().getQueries()
            val worlds = selectRandomWorlds(queries)
            val updatedWorlds = worlds.map { it.copy(randomNumber = randomIntBetween1And10000()) }
            pgPool.preparedQuery(UPDATE_WORLD_SQL)
                .executeBatch(updatedWorlds.map { Tuple.of(it.randomNumber, it.id) }).await()
            updatedWorlds
        }

        get("/plaintext").checkedCoroutineHandler {
            it.response().run {
                putCommonHeaders()
                putHeader(HttpHeaders.CONTENT_TYPE, "text/plain")
                // TODO: is reusing a single cached buffer faster?
                end("Hello, World!").await()
            }
        }
    }

    override suspend fun stop() {
        httpServer.close().await()
        pgPool.close().await()
    }
}