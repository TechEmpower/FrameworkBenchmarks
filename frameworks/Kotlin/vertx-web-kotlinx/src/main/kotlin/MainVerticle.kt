import io.netty.channel.unix.Errors
import io.netty.channel.unix.Errors.NativeIoException
import io.vertx.core.MultiMap
import io.vertx.core.buffer.Buffer
import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServer
import io.vertx.core.http.HttpServerRequest
import io.vertx.core.http.HttpServerResponse
import io.vertx.ext.web.Route
import io.vertx.ext.web.Router
import io.vertx.ext.web.RoutingContext
import io.vertx.kotlin.core.http.httpServerOptionsOf
import io.vertx.kotlin.coroutines.CoroutineRouterSupport
import io.vertx.kotlin.coroutines.CoroutineVerticle
import io.vertx.kotlin.coroutines.coAwait
import io.vertx.kotlin.pgclient.pgConnectOptionsOf
import io.vertx.pgclient.PgConnection
import io.vertx.sqlclient.PreparedQuery
import io.vertx.sqlclient.Row
import io.vertx.sqlclient.RowSet
import io.vertx.sqlclient.Tuple
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.datetime.UtcOffset
import kotlinx.datetime.format.DateTimeComponents
import kotlinx.datetime.format.format
import kotlinx.html.*
import kotlinx.html.stream.appendHTML
import kotlinx.io.buffered
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationStrategy
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.io.encodeToSink
import java.net.SocketException
import kotlin.random.Random
import kotlin.time.Clock

class MainVerticle(val hasDb: Boolean) : CoroutineVerticle(), CoroutineRouterSupport {
    object HttpHeaderValues {
        val vertxWeb = HttpHeaders.createOptimized("Vert.x-Web")
        val applicationJson = HttpHeaders.createOptimized("application/json")
        val textHtmlCharsetUtf8 = HttpHeaders.createOptimized("text/html; charset=utf-8")
        val textPlain = HttpHeaders.createOptimized("text/plain")
    }

    // `PgConnection`s as used in the "vertx" portion offers better performance than `PgPool`s.
    lateinit var pgConnection: PgConnection
    lateinit var date: String
    lateinit var httpServer: HttpServer

    lateinit var selectWorldQuery: PreparedQuery<RowSet<Row>>
    lateinit var selectFortuneQuery: PreparedQuery<RowSet<Row>>
    lateinit var updateWorldQuery: PreparedQuery<RowSet<Row>>

    val random = Random(0)

    fun setCurrentDate() {
        date = DateTimeComponents.Formats.RFC_1123.format {
            // We don't need a more complicated system `TimeZone` here (whose offset depends dynamically on the actual time due to DST) since UTC works.
            setDateTimeOffset(Clock.System.now(), UtcOffset.ZERO)
        }
    }

    override suspend fun start() {
        if (hasDb) {
            // Parameters are copied from the "vertx-web" and "vertx" portions.
            pgConnection = PgConnection.connect(
                vertx,
                pgConnectOptionsOf(
                    database = "hello_world",
                    host = "tfb-database",
                    user = "benchmarkdbuser",
                    password = "benchmarkdbpass",
                    cachePreparedStatements = true,
                    pipeliningLimit = 256
                )
            ).coAwait()

            selectWorldQuery = pgConnection.preparedQuery(SELECT_WORLD_SQL)
            selectFortuneQuery = pgConnection.preparedQuery(SELECT_FORTUNE_SQL)
            updateWorldQuery = pgConnection.preparedQuery(UPDATE_WORLD_SQL)
        }

        setCurrentDate()
        vertx.setPeriodic(1000) { setCurrentDate() }
        httpServer = vertx.createHttpServer(
            httpServerOptionsOf(port = 8080, http2ClearTextEnabled = false, strictThreadMode = true)
        )
            .requestHandler(Router.router(vertx).apply { routes() })
            .exceptionHandler {
                // wrk resets the connections when benchmarking is finished.
                if ((/* for native transport */it is NativeIoException && it.expectedErr() == Errors.ERRNO_ECONNRESET_NEGATIVE) ||
                    (/* for Java NIO */ it is SocketException && it.message == "Connection reset")
                )
                    return@exceptionHandler

                logger.info("Exception in HttpServer: $it")
                it.printStackTrace()
            }
            .listen().coAwait()
    }


    fun HttpServerRequest.getQueries(): Int {
        val queriesParam: String? = getParam("queries")
        return queriesParam?.toIntOrNull()?.coerceIn(1, 500) ?: 1
    }

    @Suppress("NOTHING_TO_INLINE")
    inline fun MultiMap.addCommonHeaders() {
        add(HttpHeaders.SERVER, HttpHeaderValues.vertxWeb)
        add(HttpHeaders.DATE, date)
    }

    @Suppress("NOTHING_TO_INLINE")
    inline fun HttpServerResponse.addJsonResponseHeaders() {
        headers().run {
            addCommonHeaders()
            add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.applicationJson)
        }
    }


    fun Route.coHandlerUnconfined(requestHandler: suspend (RoutingContext) -> Unit): Route =
        /* Some conclusions from the Plaintext test results with trailing `await()`s:
           1. `launch { /*...*/ }` < `launch(start = CoroutineStart.UNDISPATCHED) { /*...*/ }` < `launch(Dispatchers.Unconfined) { /*...*/ }`.
           1. `launch { /*...*/ }` without `context` or `start` lead to `io.netty.channel.StacklessClosedChannelException` and `io.netty.channel.unix.Errors$NativeIoException: sendAddress(..) failed: Connection reset by peer`. */
        coHandler(Dispatchers.Unconfined, requestHandler)

    inline fun <reified T : Any> Route.jsonResponseCoHandler(
        serializer: SerializationStrategy<T>,
        crossinline requestHandler: suspend (RoutingContext) -> @Serializable T
    ) =
        coHandlerUnconfined {
            it.response().run {
                addJsonResponseHeaders()

                /*
                // Approach 1
                end(json.encodeToString(serializer, requestHandler(it)))/*.coAwait()*/
                */

                /*
                // Approach 2
                // java.lang.IllegalStateException: You must set the Content-Length header to be the total size of the message body BEFORE sending any data if you are not using HTTP chunked encoding.
                toRawSink().buffered().use { bufferedSink ->
                    @OptIn(ExperimentalSerializationApi::class)
                    json.encodeToSink(serializer, requestHandler(it), bufferedSink)
                }
                */

                // Approach 3
                end(Buffer.buffer().apply {
                    toRawSink().buffered().use { bufferedSink ->
                        @OptIn(ExperimentalSerializationApi::class)
                        json.encodeToSink(serializer, requestHandler(it), bufferedSink)
                    }
                })
            }
        }

    suspend fun selectRandomWorld() =
        selectWorldQuery.execute(Tuple.of(random.nextIntBetween1And10000())).coAwait()
            .single().toWorld()

    suspend fun selectRandomWorlds(queries: Int): List<World> =
    //List(queries) { async { selectRandomWorld() } }.awaitAll()
        // This should be slightly more efficient.
        awaitAll(*Array(queries) { async { selectRandomWorld() } })


    fun Router.routes() =
        if (!hasDb) routesWithoutDb() else routesWithDb()

    fun Router.routesWithoutDb() {
        get("/json").jsonResponseCoHandler(Serializers.message) {
            Message("Hello, World!")
        }

        get("/plaintext").coHandlerUnconfined {
            it.response().run {
                headers().run {
                    addCommonHeaders()
                    add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.textPlain)
                }
                end("Hello, World!")/*.coAwait()*/
            }
        }
    }

    fun Router.routesWithDb() {
        get("/db").jsonResponseCoHandler(Serializers.world) {
            selectRandomWorld()
        }

        get("/queries").jsonResponseCoHandler(Serializers.worlds) {
            val queries = it.request().getQueries()
            selectRandomWorlds(queries)
        }

        get("/fortunes").coHandlerUnconfined {
            val fortunes = mutableListOf<Fortune>()
            selectFortuneQuery.execute().coAwait()
                .mapTo(fortunes) { it.toFortune() }

            fortunes.add(Fortune(0, "Additional fortune added at request time."))
            fortunes.sortBy { it.message }

            val htmlString = buildString {
                append("<!DOCTYPE html>")
                appendHTML(false).html {
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
            }

            it.response().run {
                headers().run {
                    addCommonHeaders()
                    add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.textHtmlCharsetUtf8)
                }
                end(htmlString)/*.coAwait()*/
            }
        }

        // Some changes to this part in the `vertx` portion in #9142 are not ported.
        get("/updates").jsonResponseCoHandler(Serializers.worlds) {
            val queries = it.request().getQueries()
            val worlds = selectRandomWorlds(queries)
            val updatedWorlds = worlds.map { it.copy(randomNumber = random.nextIntBetween1And10000()) }

            // Approach 1
            // The updated worlds need to be sorted first to avoid deadlocks.
            updateWorldQuery
                .executeBatch(updatedWorlds.sortedBy { it.id }.map { Tuple.of(it.randomNumber, it.id) }).coAwait()

            /*
            // Approach 2, worse performance
            updatedWorlds.map {
                pgPool.preparedQuery(UPDATE_WORLD_SQL).execute(Tuple.of(it.randomNumber, it.id))
            }.awaitAll()
            */

            updatedWorlds
        }
    }
}