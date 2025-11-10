import io.netty.channel.unix.Errors.NativeIoException
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
import kotlinx.html.*
import kotlinx.html.stream.appendHTML
import kotlinx.io.buffered
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationStrategy
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.io.encodeToSink
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

class MainVerticle(val hasDb: Boolean) : CoroutineVerticle(), CoroutineRouterSupport {
    // `PgConnection`s as used in the "vertx" portion offers better performance than `PgPool`s.
    lateinit var pgConnection: PgConnection
    lateinit var date: String
    lateinit var httpServer: HttpServer

    lateinit var selectWorldQuery: PreparedQuery<RowSet<Row>>
    lateinit var selectFortuneQuery: PreparedQuery<RowSet<Row>>
    lateinit var updateWordQuery: PreparedQuery<RowSet<Row>>

    fun setCurrentDate() {
        // kotlinx-datetime doesn't support the format yet.
        //date = Clock.System.now().toString()
        date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now())
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
                    pipeliningLimit = 100000
                )
            ).coAwait()

            selectWorldQuery = pgConnection.preparedQuery(SELECT_WORLD_SQL)
            selectFortuneQuery = pgConnection.preparedQuery(SELECT_FORTUNE_SQL)
            updateWordQuery = pgConnection.preparedQuery(UPDATE_WORLD_SQL)
        }

        setCurrentDate()
        vertx.setPeriodic(1000) { setCurrentDate() }
        httpServer = vertx.createHttpServer(httpServerOptionsOf(port = 8080))
            .requestHandler(Router.router(vertx).apply { routes() })
            .exceptionHandler {
                // wrk resets the connections when benchmarking is finished.
                if (
                // for epoll
                /*(it is NativeIoException && it.message == "recvAddress(..) failed: Connection reset by peer")
                || (it is SocketException && it.message == "Connection reset")*/
                // for io_uring
                    it is NativeIoException && it.message == "io_uring read(..) failed: Connection reset by peer"
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
    inline fun HttpServerResponse.putCommonHeaders() {
        putHeader(HttpHeaders.SERVER, "Vert.x-Web")
        putHeader(HttpHeaders.DATE, date)
    }

    @Suppress("NOTHING_TO_INLINE")
    inline fun HttpServerResponse.putJsonResponseHeader() {
        putCommonHeaders()
        putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
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
                putJsonResponseHeader()

                /*
                // approach 1
                end(Json.encodeToString(serializer, requestHandler(it)))/*.coAwait()*/
                */

                /*
                // approach 2
                // java.lang.IllegalStateException: You must set the Content-Length header to be the total size of the message body BEFORE sending any data if you are not using HTTP chunked encoding.
                toRawSink().buffered().use { bufferedSink ->
                    @OptIn(ExperimentalSerializationApi::class)
                    Json.encodeToSink(serializer, requestHandler(it), bufferedSink)
                }
                */

                // approach 3
                end(Buffer.buffer().apply {
                    toRawSink().buffered().use { bufferedSink ->
                        @OptIn(ExperimentalSerializationApi::class)
                        Json.encodeToSink(serializer, requestHandler(it), bufferedSink)
                    }
                })
            }
        }


    suspend fun selectRandomWorlds(queries: Int): List<World> {
        val rowSets = List(queries) {
            selectWorldQuery.execute(Tuple.of(randomIntBetween1And10000()))
        }.awaitAll()
        return rowSets.map { it.single().toWorld() }
    }

    fun Router.routes() {
        get("/json").jsonResponseCoHandler(Serializers.message) {
            Message("Hello, World!")
        }

        get("/db").jsonResponseCoHandler(Serializers.world) {
            val rowSet = selectWorldQuery.execute(Tuple.of(randomIntBetween1And10000())).coAwait()
            rowSet.single().toWorld()
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
                putCommonHeaders()
                putHeader(HttpHeaders.CONTENT_TYPE, "text/html; charset=utf-8")
                end(htmlString)/*.coAwait()*/
            }
        }

        get("/updates").jsonResponseCoHandler(Serializers.worlds) {
            val queries = it.request().getQueries()
            val worlds = selectRandomWorlds(queries)
            val updatedWorlds = worlds.map { it.copy(randomNumber = randomIntBetween1And10000()) }

            // Approach 1
            // The updated worlds need to be sorted first to avoid deadlocks.
            updateWordQuery
                .executeBatch(updatedWorlds.sortedBy { it.id }.map { Tuple.of(it.randomNumber, it.id) }).coAwait()

            /*
            // Approach 2, worse performance
            updatedWorlds.map {
                pgPool.preparedQuery(UPDATE_WORLD_SQL).execute(Tuple.of(it.randomNumber, it.id))
            }.awaitAll()
            */

            updatedWorlds
        }

        get("/plaintext").coHandlerUnconfined {
            it.response().run {
                putCommonHeaders()
                putHeader(HttpHeaders.CONTENT_TYPE, "text/plain")
                end("Hello, World!")/*.coAwait()*/
            }
        }
    }
}